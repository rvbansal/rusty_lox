use std::cell::{Cell, Ref, RefCell, RefMut};
use std::fmt;
use std::rc::Rc;

// Owns heap-allocated object and gc metadata.
pub struct GcBox<T> {
    object: RefCell<Option<T>>,
    marked: Cell<bool>,
}

// Refers to an object in the heap. B/c this a Rc to a Refcell, a GcPtr does
// not dictate whether something gets garbage collected or not.
// The user has to ensure that the object reachable through GcPtr is marked.
pub struct GcPtr<T> {
    ptr: Rc<GcBox<T>>,
}

// Defines what objects get deleted in sweep step of gc.
pub struct GcHeap<T: Traceable> {
    objects: Vec<GcPtr<T>>,
}

pub trait Traceable {
    fn trace(&self);
}

impl<T: Traceable> GcPtr<T> {
    pub fn try_borrow(&self) -> Ref<Option<T>> {
        self.ptr.object.borrow()
    }

    pub fn borrow(&self) -> Ref<T> {
        Ref::map(self.try_borrow(), |obj| {
            obj.as_ref().expect("Object was garbage collected.")
        })
    }

    pub fn try_borrow_mut(&self) -> RefMut<Option<T>> {
        self.ptr.object.borrow_mut()
    }

    pub fn borrow_mut(&mut self) -> RefMut<T> {
        RefMut::map(self.try_borrow_mut(), |obj| {
            obj.as_mut().expect("Object was garbage collected.")
        })
    }

    pub fn mark(&self) {
        if !self.ptr.marked.get() {
            self.ptr.marked.set(true);
            self.borrow().trace();
        }
    }

    fn unmark(&self) {
        self.ptr.marked.set(false);
    }

    fn marked(&self) -> bool {
        self.ptr.marked.get()
    }

    fn discard(&mut self) {
        self.try_borrow_mut().take();
    }
}

impl<T> fmt::Debug for GcPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", Rc::as_ptr(&self.ptr))
    }
}

impl<T> Clone for GcPtr<T> {
    fn clone(&self) -> Self {
        GcPtr {
            ptr: self.ptr.clone(),
        }
    }
}

impl<T> PartialEq<GcPtr<T>> for GcPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.ptr, &other.ptr)
    }
}

impl<T> Eq for GcPtr<T> {}

impl<T: Traceable> GcHeap<T> {
    pub fn new() -> Self {
        GcHeap { objects: vec![] }
    }

    pub fn insert(&mut self, obj: T) -> GcPtr<T> {
        let gc_obj_box = GcBox {
            object: RefCell::new(Some(obj)),
            marked: Cell::new(false),
        };

        let gc_obj_ptr = GcPtr {
            ptr: Rc::new(gc_obj_box),
        };

        // Make an internal copy of pointer before returning a ptr to caller
        self.objects.push(gc_obj_ptr.clone());
        gc_obj_ptr
    }

    pub fn sweep(&mut self) {
        for ptr in self.objects.iter_mut() {
            if !ptr.marked() {
                ptr.discard();
            }
        }

        // Remove deleted objects.
        self.objects.retain(|ptr| ptr.marked());

        // Unmark retained objects.
        for ptr in self.objects.iter() {
            ptr.unmark();
        }
    }
}

impl<T: Traceable> Drop for GcHeap<T> {
    // Kill everything in heap when GcHeap goes out of scope.
    fn drop(&mut self) {
        for ptr in self.objects.iter_mut() {
            ptr.discard();
        }
    }
}

#[cfg(tests)]
mod tests {
    use super::*;

    impl Traceable for String {
        fn trace(&self) {}
    }

    struct TestStruct {
        next: Option<GcPtr<TestStruct>>,
    }

    impl TestStruct {
        fn new() -> Self {
            TestStruct { next: None }
        }

        fn new_with_next_struct(ns: GcPtr<TestStruct>) -> Self {
            TestStruct { next: ns }
        }
    }

    impl Traceable for TestStruct {
        fn trace(&self) {
            if let Some(next) = &self.next {
                next.mark();
            }
        }
    }
}
