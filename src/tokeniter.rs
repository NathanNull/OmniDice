use std::fmt::Display;

use crate::error::LineIndex;

#[derive(Debug, Clone, Copy)]
pub enum TokenWidth {
    Wide(usize),
    Tall(usize, usize),
}

impl Display for TokenWidth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Wide(w) => write!(f, "{w}"),
            Self::Tall(h, post_w) => write!(f, "{h}x{post_w}"),
        }
    }
}

#[derive(Clone)]
pub struct TokenIter<I: std::fmt::Debug, T: Iterator<Item = (I, TokenWidth)>> {
    inner: T,
    peeked: Vec<(I, TokenWidth)>,
    pub(crate) pos: LineIndex,
    pos_memory: Vec<LineIndex>,
}

impl<I: std::fmt::Debug, T: Iterator<Item = (I, TokenWidth)>> Iterator for TokenIter<I, T> {
    type Item = I;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_raw().map(|(t, _)| t)
    }
}

impl<I: std::fmt::Debug, T: Iterator<Item = (I, TokenWidth)>> TokenIter<I, T> {
    pub fn new(inner: T) -> Self {
        Self {
            inner,
            peeked: vec![],
            pos: LineIndex(1, 1),
            pos_memory: vec![],
        }
    }

    fn next_raw(&mut self) -> Option<(I, TokenWidth)> {
        if let Some(peeked) = self.peeked.pop() {
            self.step(peeked.1, true);
            Some(peeked)
        } else if let Some(n) = self.inner.next() {
            self.step(n.1, true);
            Some(n)
        } else {
            None
        }
    }

    fn step(&mut self, w: TokenWidth, forward: bool) {
        match w {
            TokenWidth::Tall(height, post_width) => {
                if forward {
                    self.pos_memory.push(self.pos);
                    self.pos.0 += height;
                    self.pos.1 = post_width;
                } else {
                    self.pos = self
                        .pos_memory
                        .pop()
                        .expect("Can't backtrack multiline token that wasn't already there")
                }
            }
            TokenWidth::Wide(width) => {
                if forward {
                    self.pos.1 += width
                } else {
                    self.pos.1 -= width
                }
            }
        }
    }

    pub fn peek(&mut self) -> Option<&I> {
        if self.peeked.is_empty() {
            if let Some(peeked) = self.inner.next() {
                self.peeked.push(peeked)
            }
        }
        self.peeked.last().map(|(t, _)| t)
    }

    pub fn replace(&mut self, itm: I) {
        self.peeked.push((itm, TokenWidth::Wide(0)));
    }

    pub fn replace_sized(&mut self, itm: I, size: TokenWidth) {
        self.peeked.push((itm, size));
        self.step(size, false);
    }

    pub fn eat(&mut self, pattern: impl IntoIterator<Item = I>) -> Option<Vec<I>>
    where
        I: PartialEq,
    {
        let mut removed = vec![];
        for itm in pattern {
            if let Some((n, sz)) = self.next_raw() {
                if n != itm {
                    self.replace_sized(n, sz);
                    for (r, sr) in removed.into_iter().rev() {
                        self.replace_sized(r, sr);
                    }
                    return None;
                }
                removed.push((n, sz))
            } else {
                for (r, sr) in removed.into_iter().rev() {
                    self.replace_sized(r, sr);
                }
                return None;
            }
        }
        Some(removed.into_iter().map(|(t, _)| t).collect())
    }

    pub fn expect(&mut self, ele: I) -> Result<(), String>
    where
        I: PartialEq,
    {
        match self.next() {
            Some(next) if next == ele => Ok(()),
            Some(next) => Err(format!("Expected {ele:?}, found {next:?}")),
            None => Err("Unexpected EOF".to_string()),
        }
    }
}

impl<T: Iterator<Item = (char, TokenWidth)>> TokenIter<char, T> {
    pub fn eat_str(&mut self, str: &str) -> bool {
        self.eat(str.chars()).is_some()
    }
}
