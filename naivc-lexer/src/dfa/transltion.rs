use std::{fmt::Debug, ops::RangeInclusive};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LabelledTransition<T: Debug + Ord + Eq> {
    pub range: RangeInclusive<char>,
    pub target: T,
}

impl<T: Debug + Ord + Eq> Ord for LabelledTransition<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.range.start().cmp(other.range.start()) {
            std::cmp::Ordering::Equal => match self.range.end().cmp(other.range.end()) {
                std::cmp::Ordering::Equal => self.target.cmp(&other.target),
                ord => ord,
            },
            ord => ord,
        }
    }
}

impl<T: Debug + Ord + Eq> PartialOrd for LabelledTransition<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
