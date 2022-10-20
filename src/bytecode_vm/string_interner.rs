use std::collections::HashSet;
use std::rc::Rc;

pub type StringIntern = Rc<str>;

pub struct StringInterner {
    table: HashSet<Rc<str>>,
}

impl StringInterner {
    pub fn new() -> Self {
        StringInterner {
            table: HashSet::new(),
        }
    }

    pub fn get_string_intern<S>(&mut self, string: S) -> StringIntern
    where
        S: AsRef<str>,
    {
        let string_slice = string.as_ref();
        match self.table.get(string_slice) {
            Some(rc) => rc.clone(),
            None => {
                let rc = Rc::<str>::from(string_slice);
                self.table.insert(rc.clone());
                rc
            }
        }
    }

    pub fn insert<S>(&mut self, string: S)
    where
        S: AsRef<str>,
    {
        self.get_string_intern(string);
    }

    pub fn remove_no_ref_strings(&mut self) {
        self.table.retain(|rc| Rc::strong_count(rc) > 1);
    }

    pub fn len(&self) -> usize {
        self.table.len()
    }
}
