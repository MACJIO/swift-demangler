pub fn is_digit(c: char) -> bool {
    '0' <= c && c <= '9'
}

pub fn is_upper_letter(c: char) -> bool {
    'A' <= c && c <= 'Z'
}

pub fn is_lower_letter(c: char) -> bool {
    'a' <= c && c <= 'z'
}

pub fn is_letter(c: char) -> bool {
    is_upper_letter(c) || is_lower_letter(c)
}

pub fn is_word_start(c: char) -> bool {
    !(is_digit(c) || c == '_' || c == '\x00')
}

pub fn is_word_end(c: char, pc: char) -> bool {
    c == '_' || c == '\x00' || (!is_upper_letter(pc) && is_upper_letter(c))
}