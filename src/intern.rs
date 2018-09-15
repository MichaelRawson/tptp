use std::collections::HashSet;
use std::sync::Arc;

pub struct StringCache {
    strings: HashSet<Arc<String>>
}

impl StringCache {
    pub fn new() -> Self {
        StringCache {
            strings: HashSet::new()
        }
    }

    pub fn intern(&mut self, string: String) -> Arc<String> {
        let item = Arc::new(string);
        let entry = self.strings.get(&item).map(|x| x.clone());
        match entry {
            Some(interned) => interned,
            None => {
                self.strings.insert(item.clone());
                item
            }
        }
    }
}
