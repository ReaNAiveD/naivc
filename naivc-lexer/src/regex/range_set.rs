use std::ops::RangeInclusive;

#[derive(Debug, Clone, PartialEq)]
pub struct RangeSet {
    ranges: Vec<RangeInclusive<char>>, // Manually sorted and merged
}

impl RangeSet {
    pub fn empty() -> Self {
        Self { ranges: Vec::new() }
    }

    pub fn from_char(c: char) -> Self {
        Self {
            ranges: vec![c..=c],
        }
    }

    pub fn from_range(start: char, end: char) -> Self {
        Self {
            ranges: vec![start..=end],
        }
    }

    pub fn from_ranges(mut ranges: Vec<RangeInclusive<char>>) -> Self {
        ranges.sort_by_key(|r| *r.start());
        Self {
            ranges: Self::merge_ranges(ranges),
        }
    }

    pub fn from_negated_ranges(mut ranges: Vec<RangeInclusive<char>>) -> Self {
        ranges.sort_by_key(|r| *r.start());
        let ranges = Self::merge_ranges(ranges);
        let negated_ranges = Self::complement_sorted_ranges(ranges);
        Self {
            ranges: negated_ranges,
        }
    }

    pub fn add(&self, other: Self) -> Self {
        let all_ranges: Vec<_> = self.ranges.iter().cloned().chain(other.ranges).collect();

        Self {
            ranges: Self::merge_ranges(all_ranges),
        }
    }

    pub fn add_negated(&self, other: Self) -> Self {
        let negated_ranges = Self::complement_sorted_ranges(other.ranges);

        let all_ranges: Vec<_> = self.ranges.iter().cloned().chain(negated_ranges).collect();

        Self {
            ranges: Self::merge_ranges(all_ranges),
        }
    }

    /// Helper function to merge sorted ranges
    fn merge_ranges(mut ranges: Vec<RangeInclusive<char>>) -> Vec<RangeInclusive<char>> {
        ranges.sort_by_key(|r| *r.start());
        let mut result: Vec<RangeInclusive<char>> = Vec::new();

        for range in ranges {
            if let Some(last) = result.last_mut() {
                let last_end_code = *last.end() as u32;
                let range_start_code = *range.start() as u32;
                if last_end_code + 1 >= range_start_code {
                    *last = *last.start()..=(*last.end()).max(*range.end());
                    continue;
                }
            }
            result.push(range);
        }
        result
    }

    /// Helper function to compute complement of ranges
    fn complement_sorted_ranges(ranges: Vec<RangeInclusive<char>>) -> Vec<RangeInclusive<char>> {
        let mut result = Vec::new();
        let mut last_end = 0u32;
        for range in ranges {
            let range_start = *range.start() as u32;
            let range_end = *range.end() as u32;

            if range_start > last_end {
                result.push(
                    char::from_u32(last_end).unwrap()..=char::from_u32(range_start - 1).unwrap(),
                );
            }
            last_end = range_end + 1;
        }
        if last_end <= char::MAX as u32 {
            result.push(char::from_u32(last_end).unwrap()..=char::MAX);
        }
        result
    }

    pub fn add_char(&self, c: char) -> Self {
        self.add_range(c, c)
    }

    pub fn add_range(&self, start: char, end: char) -> Self {
        let mut updated_ranges = Vec::new();
        let mut new_range = start..=end;

        for existing_range in &self.ranges {
            let new_end_code = *new_range.end() as u32;
            let new_start_code = *new_range.start() as u32;
            let existing_start_code = *existing_range.start() as u32;
            let existing_end_code = *existing_range.end() as u32;

            if new_end_code + 1 < existing_start_code || new_start_code > existing_end_code + 1 {
                updated_ranges.push(existing_range.clone());
            } else {
                let start = (*new_range.start()).min(*existing_range.start());
                let end = (*new_range.end()).max(*existing_range.end());
                new_range = start..=end;
            }
        }

        // Insert the new range in sorted position
        let insert_pos = updated_ranges
            .iter()
            .position(|r| r.start() > new_range.start())
            .unwrap_or(updated_ranges.len());
        updated_ranges.insert(insert_pos, new_range);

        Self {
            ranges: updated_ranges,
        }
    }

    pub fn remove_char(&self, c: char) -> Self {
        self.remove_range(c, c)
    }

    pub fn remove_range(&self, start: char, end: char) -> Self {
        let remove_range = start..=end;
        let mut updated_ranges = Vec::new();

        for existing_range in &self.ranges {
            if *remove_range.end() < *existing_range.start()
                || *remove_range.start() > *existing_range.end()
            {
                updated_ranges.push(existing_range.clone());
            } else {
                if *remove_range.start() > *existing_range.start() {
                    let end_code = (*remove_range.start() as u32) - 1;
                    if let Some(end_char) = char::from_u32(end_code) {
                        updated_ranges.push(*existing_range.start()..=end_char);
                    }
                }
                if *remove_range.end() < *existing_range.end() {
                    let start_code = (*remove_range.end() as u32) + 1;
                    if let Some(start_char) = char::from_u32(start_code) {
                        updated_ranges.push(start_char..=*existing_range.end());
                    }
                }
            }
        }

        Self {
            ranges: updated_ranges,
        }
    }

    pub fn contains(&self, c: char) -> bool {
        for range in &self.ranges {
            if range.contains(&c) {
                return true;
            } else if range.start() > &c {
                break;
            }
        }
        false
    }

    pub fn negate(self) -> Self {
        Self {
            ranges: Self::complement_sorted_ranges(self.ranges),
        }
    }
}

impl Default for RangeSet {
    fn default() -> Self {
        Self::empty()
    }
}

impl RangeSet {
    pub fn unicode_letter() -> Self {
        Self::from_ranges(
            crate::unicode_tables::general_category::LETTER
                .iter()
                .map(|(from, to)| *from..=*to)
                .collect(),
        )
    }

    pub fn unicode_digit() -> Self {
        Self::from_ranges(
            crate::unicode_tables::general_category::DECIMAL_NUMBER
                .iter()
                .map(|(from, to)| *from..=*to)
                .collect(),
        )
    }

    pub fn unicode_space() -> Self {
        Self::from_ranges(
            crate::unicode_tables::general_category::SEPARATOR
                .iter()
                .map(|(from, to)| *from..=*to)
                .collect(),
        )
    }

    pub fn unicode_xid_start() -> Self {
        Self::from_ranges(
            crate::unicode_tables::property_bool::XID_START
                .iter()
                .map(|(from, to)| *from..=*to)
                .collect(),
        )
    }

    pub fn unicode_xid_continue() -> Self {
        Self::from_ranges(
            crate::unicode_tables::property_bool::XID_CONTINUE
                .iter()
                .map(|(from, to)| *from..=*to)
                .collect(),
        )
    }

    pub fn unicode_upper() -> Self {
        Self::from_ranges(
            crate::unicode_tables::general_category::UPPERCASE_LETTER
                .iter()
                .map(|(from, to)| *from..=*to)
                .collect(),
        )
    }

    pub fn unicode_lower() -> Self {
        Self::from_ranges(
            crate::unicode_tables::general_category::LOWERCASE_LETTER
                .iter()
                .map(|(from, to)| *from..=*to)
                .collect(),
        )
    }
}

impl RangeSet {
    pub fn iter(&self) -> impl Iterator<Item = &RangeInclusive<char>> {
        self.ranges.iter()
    }
}