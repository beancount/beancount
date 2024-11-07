// pub fn add(left: u64, right: u64) -> u64 {
//     left + right
// }

// Include the `items` module, which is generated from items.proto.
// It is important to maintain the same structure as in the proto.
pub mod beancount {
    include!(concat!(env!("OUT_DIR"), "/beancount.rs"));
    pub mod precision {
        include!(concat!(env!("OUT_DIR"), "/beancount.precision.rs"));
    }
    pub mod inter {
        include!(concat!(env!("OUT_DIR"), "/beancount.inter.rs"));
    }
    pub mod options {
        include!(concat!(env!("OUT_DIR"), "/beancount.options.rs"));
    }
}

use beancount::*;

/// Returns a large shirt of the specified color
pub fn create_transaction() -> Option<Transaction> {
    let txn = Transaction::default();
    // shirt.color = color;
    // shirt.set_size(items::shirt::Size::Large);
    // shirt
    Some(txn)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let _ = create_transaction();
    }
}
