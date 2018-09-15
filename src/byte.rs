pub fn printable(c: u8) -> bool {
    match c {
        b' '...b'~' => true,
        _ => false,
    }
}

pub fn numeric(c: u8) -> bool {
    match c {
        b'0'...b'9' => true,
        _ => false,
    }
}

pub fn alphanumeric(c: u8) -> bool {
    match c {
        b'a'...b'z' | b'A'...b'Z' | b'0'...b'9' | b'_' => true,
        _ => false,
    }
}
