use std::cell::Cell;

#[derive(Debug)]
pub struct Gc<T> {
    index: usize,
    _marker: std::marker::PhantomData<*const T>,
}

#[derive(Debug)]
pub struct SubHeap<T> {
    objects: Vec<Option<HeapEntry<T>>>,
    free_list: Vec<usize>,
}

#[derive(Debug)]
struct HeapEntry<T> {
    content: T,
    marked: Cell<bool>,
}

impl<T> Gc<T> {
    fn new(index: usize) -> Self {
        Self {
            index,
            _marker: Default::default(),
        }
    }
}

impl<T> Copy for Gc<T> {}
impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> SubHeap<T> {
    pub fn new() -> Self {
        Self {
            objects: vec![],
            free_list: vec![],
        }
    }

    pub fn manage(&mut self, value: T) -> Gc<T> {
        let entry = Some(HeapEntry::new(value));
        match self.free_list.pop() {
            Some(index) => {
                self.objects[index] = entry;
                Gc::new(index)
            }
            None => {
                let index = self.objects.len();
                self.objects.push(entry);
                Gc::new(index)
            }
        }
    }

    fn get_entry(&self, ptr: Gc<T>) -> &HeapEntry<T> {
        let length = self.objects.len();
        match self.objects.get(ptr.index) {
            None => panic!(
                "gc pointed outside vector bounds: {} > {}",
                ptr.index, length
            ),
            Some(None) => panic!("gc pointed to freed object: {}", ptr.index),
            Some(Some(x)) => x,
        }
    }

    fn get_entry_mut(&mut self, ptr: Gc<T>) -> &mut HeapEntry<T> {
        let length = self.objects.len();
        match self.objects.get_mut(ptr.index) {
            Some(Some(x)) => x,
            Some(None) => panic!("gc pointed to freed object: {}", ptr.index),
            None => panic!(
                "gc pointed outside vector bounds: {} > {}",
                ptr.index, length
            ),
        }
    }

    pub fn get(&self, ptr: Gc<T>) -> &T {
        &self.get_entry(ptr).content
    }

    pub fn get_mut(&mut self, ptr: Gc<T>) -> &mut T {
        &mut self.get_entry_mut(ptr).content
    }

    pub fn mark_and_trace<'a, F>(&'a self, ptr: Gc<T>, trace: F)
    where
        F: Fn(&'a T),
    {
        let entry = self.get_entry(ptr);
        if !entry.mark() {
            trace(&entry.content)
        }
    }

    pub fn sweep(&mut self) {
        for (index, slot) in self.objects.iter_mut().enumerate() {
            if let Some(entry) = slot {
                if entry.is_marked() {
                    entry.unmark();
                } else {
                    *slot = None;
                    self.free_list.push(index);
                }
            }
        }
    }

    pub fn size(&self) -> usize {
        self.objects.len() * std::mem::size_of::<T>()
    }
}

impl<T> HeapEntry<T> {
    fn new(content: T) -> Self {
        Self {
            content,
            marked: Cell::new(false),
        }
    }
}

impl<T> HeapEntry<T> {
    pub fn mark(&self) -> bool {
        self.marked.replace(true)
    }

    pub fn unmark(&self) {
        self.marked.set(false);
    }

    pub fn is_marked(&self) -> bool {
        self.marked.get()
    }
}

pub trait Heap {
    fn sweep(&mut self);
}

pub trait HasSubHeap<T>: Heap {
    fn get_subheap(&self) -> &SubHeap<T>;
    fn get_subheap_mut(&mut self) -> &mut SubHeap<T>;
    fn trace(&self, content: &T);
}

pub trait Manages<T> {
    fn manage(&mut self, content: T) -> Gc<T>;
    fn get(&self, ptr: Gc<T>) -> &T;
    fn get_mut(&mut self, ptr: Gc<T>) -> &mut T;
    fn mark(&self, ptr: Gc<T>);
}

impl<H, T> Manages<T> for H
where
    H: HasSubHeap<T>,
{
    fn manage(&mut self, content: T) -> Gc<T> {
        self.get_subheap_mut().manage(content)
    }

    fn get(&self, ptr: Gc<T>) -> &T {
        self.get_subheap().get(ptr)
    }

    fn get_mut(&mut self, ptr: Gc<T>) -> &mut T {
        self.get_subheap_mut().get_mut(ptr)
    }

    fn mark(&self, ptr: Gc<T>) {
        self.get_subheap()
            .mark_and_trace(ptr, |content| self.trace(content))
    }
}
