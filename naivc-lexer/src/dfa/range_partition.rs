use std::{cmp::Ordering, collections::BinaryHeap, ops::RangeInclusive};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EventType {
    Start,
    End,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RangeEvent {
    position: char,
    event_type: EventType,
}

impl RangeEvent {
    fn new(position: char, event_type: EventType) -> Self {
        Self {
            position,
            event_type,
        }
    }
}

impl PartialOrd for RangeEvent {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for RangeEvent {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.position.cmp(&other.position) {
            Ordering::Equal => {
                // END events come before START events at the same position
                match (self.event_type, other.event_type) {
                    (EventType::End, EventType::Start) => Ordering::Less,
                    (EventType::Start, EventType::End) => Ordering::Greater,
                    _ => Ordering::Equal,
                }
            }
            ord => ord,
        }
    }
}

pub struct RangePartitioner {
    sorted_events: Vec<RangeEvent>,
    next_event_index: usize,
    depth: usize,
    current_position: Option<char>,
}

impl RangePartitioner {
    pub fn new<'a>(
        set: impl Iterator<Item = &'a RangeInclusive<char>>,
    ) -> Self {
        let mut events = Vec::new();

        for range in set {
            events.push(RangeEvent::new(*range.start(), EventType::Start));
            if let Some(next) = next_char(*range.end()) {
                events.push(RangeEvent::new(next, EventType::End));
            }
        }

        events.sort();

        Self {
            sorted_events: events,
            next_event_index: 0,
            depth: 0,
            current_position: None,
        }
    }

    fn peek_next_position(&self) -> Option<char> {
        self.sorted_events
            .get(self.next_event_index)
            .map(|event| event.position)
    }

    fn process_events_at_position(&mut self, position: char) {
        while let Some(event) = self.sorted_events.get(self.next_event_index) {
            if event.position != position {
                break;
            }

            match event.event_type {
                EventType::Start => self.depth += 1,
                EventType::End => self.depth -= 1,
            }

            self.next_event_index += 1;
        }
    }
}

impl Iterator for RangePartitioner {
    type Item = RangeInclusive<char>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(start_pos) = self.current_position {
                if self.depth > 0 {
                    let end_char = if let Some(next_pos) = self.peek_next_position() {
                        prev_char(next_pos)?
                    } else {
                        char::MAX
                    };

                    if let Some(next_pos) = self.peek_next_position() {
                        self.process_events_at_position(next_pos);
                        self.current_position = Some(next_pos);
                    } else {
                        self.current_position = None;
                    }

                    if start_pos <= end_char {
                        return Some(start_pos..=end_char);
                    } else {
                        continue;
                    }
                }
            }

            if let Some(next_pos) = self.peek_next_position() {
                self.process_events_at_position(next_pos);
                self.current_position = Some(next_pos);
            } else {
                return None;
            }
        }
    }
}

/// A wrapper for events in the min-heap that tracks which RangeSet they came from.
#[derive(Debug, Clone)]
struct HeapEvent {
    event: RangeEvent,
    set_index: usize,
    range_index: usize,
}

impl PartialEq for HeapEvent {
    fn eq(&self, other: &Self) -> bool {
        self.event == other.event
    }
}

impl Eq for HeapEvent {}

impl PartialOrd for HeapEvent {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for HeapEvent {
    fn cmp(&self, other: &Self) -> Ordering {
        // Reverse order for min-heap (BinaryHeap is a max-heap by default)
        other.event.cmp(&self.event)
    }
}

/// An iterator that partitions multiple `RangeSet`s using a sweep-line algorithm with min-heap.
///
/// This uses an event-based approach where each range is converted to START and END events,
/// then uses a min-heap to efficiently merge the sorted event streams from each RangeSet.
/// This achieves O(n log k) complexity where n is the total number of events and k is the
/// number of RangeSets, which is better than O(n log n) when k << n.
/// Partitions multiple sorted `RangeInclusive<char>` sets using a heap-based sweep line.
///
/// All sets provided to `SortedRangePartitioner::new` must contain sorted, non-overlapping
/// ranges to maintain correctness.
pub struct SortedRangePartitioner {
    heap: BinaryHeap<HeapEvent>,
    sets: Vec<Vec<RangeInclusive<char>>>,
    set_positions: Vec<usize>,
    depth: usize,
    current_position: Option<char>,
}

impl SortedRangePartitioner {
    pub fn new<'a>(
        sets: impl Iterator<Item = impl Iterator<Item = &'a RangeInclusive<char>>>,
    ) -> Self {
        let sets_vec: Vec<Vec<RangeInclusive<char>>> =
            sets.map(|s| s.map(|r| r.clone()).collect()).collect();
        let set_count = sets_vec.len();
        let mut heap = BinaryHeap::new();
        let set_positions = vec![0; set_count];

        // Initialize heap with the first range from each set
        for (set_index, ranges) in sets_vec.iter().enumerate() {
            if let Some(range) = ranges.first() {
                heap.push(HeapEvent {
                    event: RangeEvent::new(*range.start(), EventType::Start),
                    set_index,
                    range_index: 0,
                });
            }
        }

        Self {
            heap,
            sets: sets_vec,
            set_positions,
            depth: 0,
            current_position: None,
        }
    }

    fn add_end_event_for_range(&mut self, set_index: usize, range_index: usize) {
        if let Some(range) = self.sets[set_index].get(range_index) {
            if let Some(next) = next_char(*range.end()) {
                self.heap.push(HeapEvent {
                    event: RangeEvent::new(next, EventType::End),
                    set_index,
                    range_index,
                });
            }
        }
    }

    fn add_next_range_from_set(&mut self, set_index: usize) {
        self.set_positions[set_index] += 1;
        let next_range_index = self.set_positions[set_index];

        if let Some(range) = self.sets[set_index].get(next_range_index) {
            self.heap.push(HeapEvent {
                event: RangeEvent::new(*range.start(), EventType::Start),
                set_index,
                range_index: next_range_index,
            });
        }
    }

    fn process_events_at_position(&mut self, position: char) {
        // Collect all events at this position
        let mut events_at_position = Vec::new();

        while let Some(heap_event) = self.heap.peek() {
            if heap_event.event.position == position {
                let heap_event = self.heap.pop().unwrap();
                events_at_position.push(heap_event);
            } else {
                break;
            }
        }

        // Process events: first all ENDs, then all STARTs (as per RangeEvent ordering)
        events_at_position.sort_by(|a, b| a.event.cmp(&b.event));

        for heap_event in events_at_position {
            match heap_event.event.event_type {
                EventType::Start => {
                    self.depth += 1;
                    // Add the corresponding END event for this range
                    self.add_end_event_for_range(heap_event.set_index, heap_event.range_index);
                }
                EventType::End => {
                    self.depth -= 1;
                    // Add the next range from this set
                    self.add_next_range_from_set(heap_event.set_index);
                }
            }
        }
    }
}

impl Iterator for SortedRangePartitioner {
    type Item = RangeInclusive<char>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // If we have a current position and depth > 0, we're in a range
            if let Some(start_pos) = self.current_position {
                if self.depth > 0 {
                    // Find the end of this partition
                    let end_char = if let Some(next_event) = self.heap.peek() {
                        prev_char(next_event.event.position)?
                    } else {
                        char::MAX
                    };

                    // Move to the next position
                    if let Some(next_event) = self.heap.peek() {
                        let next_pos = next_event.event.position;
                        self.process_events_at_position(next_pos);
                        self.current_position = Some(next_pos);
                    } else {
                        self.current_position = None;
                    }

                    return Some(start_pos..=end_char);
                }
            }

            // Find the next position where depth > 0
            if let Some(next_event) = self.heap.peek() {
                let next_pos = next_event.event.position;
                self.process_events_at_position(next_pos);
                self.current_position = Some(next_pos);
            } else {
                return None;
            }
        }
    }
}

/// Returns the character immediately before the given character.
///
/// # Arguments
///
/// * `c` - The character whose predecessor to find
///
/// # Returns
///
/// * `Some(char)` - The previous character if it exists
/// * `None` - If `c` is the first character (U+0000)
fn prev_char(c: char) -> Option<char> {
    let code = c as u32;
    if code > 0 {
        char::from_u32(code - 1)
    } else {
        None
    }
}

/// Returns the character immediately after the given character.
///
/// # Arguments
///
/// * `c` - The character whose successor to find
///
/// # Returns
///
/// * `Some(char)` - The next character if it exists
/// * `None` - If `c` is the last valid Unicode character
fn next_char(c: char) -> Option<char> {
    let code = c as u32;
    if code < char::MAX as u32 {
        char::from_u32(code + 1)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use std::ops::RangeInclusive;

    use super::*;

    fn collect_sorted_partitions(
        sets: &[&Vec<RangeInclusive<char>>],
    ) -> Vec<RangeInclusive<char>> {
        SortedRangePartitioner::new(sets.iter().map(|set| set.iter())).collect()
    }

    fn collect_range_partitions(
        sets: &[&Vec<RangeInclusive<char>>],
    ) -> Vec<RangeInclusive<char>> {
        RangePartitioner::new(sets.iter().flat_map(|set| set.iter())).collect()
    }

    #[test]
    fn test_partition_two_overlapping_ranges() {
        let set_a = vec!['a'..='c'];
        let set_b = vec!['b'..='d'];
        let sets = [&set_a, &set_b];
        let expected = vec!['a'..='a', 'b'..='c', 'd'..='d'];

        let partitions = collect_sorted_partitions(&sets);
        assert_eq!(partitions, expected);

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, expected);
    }

    #[test]
    fn test_partition_non_overlapping_ranges() {
        let set_a = vec!['a'..='c'];
        let set_b = vec!['e'..='g'];
        let sets = [&set_a, &set_b];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(partitions, vec!['a'..='c', 'e'..='g']);

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_contained_range() {
        let set_a = vec!['a'..='f'];
        let set_b = vec!['c'..='d'];
        let sets = [&set_a, &set_b];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(partitions, vec!['a'..='b', 'c'..='d', 'e'..='f']);

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_three_ranges() {
        let set_a = vec!['a'..='d'];
        let set_b = vec!['c'..='f'];
        let set_c = vec!['e'..='h'];
        let sets = [&set_a, &set_b, &set_c];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(
            partitions,
            vec![
                'a'..='b', // only A
                'c'..='d', // A and B
                'e'..='f', // B and C
                'g'..='h'  // only C
            ]
        );

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_identical_ranges() {
        let set_a = vec!['a'..='c'];
        let set_b = vec!['a'..='c'];
        let sets = [&set_a, &set_b];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(partitions, vec!['a'..='c']);

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_single_chars() {
        let set_a = vec!['a'..='a'];
        let set_b = vec!['b'..='b'];
        let set_c = vec!['c'..='c'];
        let sets = [&set_a, &set_b, &set_c];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(partitions, vec!['a'..='a', 'b'..='b', 'c'..='c']);

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_adjacent_ranges() {
        let set_a = vec!['a'..='c'];
        let set_b = vec!['d'..='f'];
        let sets = [&set_a, &set_b];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(partitions, vec!['a'..='c', 'd'..='f']);

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_empty_set() {
        let set_a = vec!['a'..='c'];
        let set_b: Vec<RangeInclusive<char>> = vec![];
        let sets = [&set_a, &set_b];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(partitions, vec!['a'..='c']);

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_multiple_ranges_per_set() {
        let set_a = vec!['a'..='c', 'g'..='i'];
        let set_b = vec!['b'..='d', 'h'..='j'];
        let sets = [&set_a, &set_b];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(
            partitions,
            vec![
                'a'..='a', // only A
                'b'..='c', // A and B overlap
                'd'..='d', // only B
                'g'..='g', // only A
                'h'..='i', // A and B overlap
                'j'..='j'  // only B
            ]
        );

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_complex_overlap() {
        // This test demonstrates how the partitioner handles complex overlapping scenarios.
        // A: 'a'..='j', B: 'c'..='g', C: 'e'..='k'
        let set_a = vec!['a'..='j'];
        let set_b = vec!['c'..='g'];
        let set_c = vec!['e'..='k'];
        let sets = [&set_a, &set_b, &set_c];

        let partitions = collect_sorted_partitions(&sets);

        // Verify ranges are in order and non-overlapping
        for window in partitions.windows(2) {
            assert!(*window[0].end() < *window[1].start());
        }

        // Verify coverage - should start at 'a' and end at 'k'
        assert_eq!(*partitions.first().unwrap().start(), 'a');
        assert_eq!(*partitions.last().unwrap().end(), 'k');
        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_single_set() {
        let set_a = vec!['a'..='c', 'e'..='g'];
        let sets = [&set_a];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(partitions, vec!['a'..='c', 'e'..='g']);

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_no_sets() {
        let sets: Vec<&Vec<RangeInclusive<char>>> = Vec::new();
        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(partitions, Vec::<RangeInclusive<char>>::new());

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_with_char_max() {
        // Test range ending at char::MAX
        let set_a = vec!['a'..='c'];
        let set_b = vec!['b'..=char::MAX];
        let sets = [&set_a, &set_b];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(
            partitions,
            vec![
                'a'..='a',       // only A
                'b'..='c',       // A and B
                'd'..=char::MAX  // only B, extends to end
            ]
        );

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_multiple_ranges_ending_at_char_max() {
        let set_a = vec!['a'..='c', 'x'..=char::MAX];
        let set_b = vec!['b'..='d', 'y'..=char::MAX];
        let sets = [&set_a, &set_b];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(
            partitions,
            vec![
                'a'..='a',       // only A
                'b'..='c',       // A and B
                'd'..='d',       // only B
                'x'..='x',       // only A
                'y'..=char::MAX  // A and B, extends to end
            ]
        );

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_all_ranges_end_at_char_max() {
        let set_a = vec!['a'..=char::MAX];
        let set_b = vec!['c'..=char::MAX];
        let set_c = vec!['e'..=char::MAX];
        let sets = [&set_a, &set_b, &set_c];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(
            partitions,
            vec![
                'a'..='b',       // only A
                'c'..='d',       // A and B
                'e'..=char::MAX  // A, B, and C
            ]
        );

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_single_char_at_char_max() {
        let set_a = vec![char::MAX..=char::MAX];
        let set_b = vec!['a'..='c'];
        let sets = [&set_a, &set_b];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(
            partitions,
            vec![
                'a'..='c',             // only B
                char::MAX..=char::MAX  // only A
            ]
        );

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_overlapping_at_char_max() {
        // Create a range that ends just before char::MAX and one that includes char::MAX
        let almost_max = char::from_u32(char::MAX as u32 - 1).unwrap();
        let set_a = vec!['a'..=almost_max];
        let set_b = vec![almost_max..=char::MAX];
        let sets = [&set_a, &set_b];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(
            partitions,
            vec![
                'a'..=char::from_u32(almost_max as u32 - 1).unwrap(), // only A
                almost_max..=almost_max,                              // A and B
                char::MAX..=char::MAX                                 // only B
            ]
        );

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_many_overlapping_ranges() {
        // Test with many ranges that create complex partitions
        let set_a = vec!['a'..='e', 'k'..='o', 'u'..='z'];
        let set_b = vec!['c'..='g', 'm'..='q', 'w'..='z'];
        let set_c = vec!['e'..='i', 'o'..='s', 'y'..='z'];
        let sets = [&set_a, &set_b, &set_c];

        let partitions = collect_sorted_partitions(&sets);

        // Verify all partitions are non-overlapping
        for window in partitions.windows(2) {
            assert!(*window[0].end() < *window[1].start());
        }

        // Verify correct number of partitions
        assert_eq!(partitions.len(), 13);

        // Verify coverage
        assert_eq!(*partitions.first().unwrap().start(), 'a');
        assert_eq!(*partitions.last().unwrap().end(), 'z');

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_interleaved_ranges() {
        // Test ranges that interleave without direct overlap
        let set_a = vec!['a'..='b', 'e'..='f', 'i'..='j'];
        let set_b = vec!['c'..='d', 'g'..='h', 'k'..='l'];
        let sets = [&set_a, &set_b];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(
            partitions,
            vec![
                'a'..='b', // A
                'c'..='d', // B
                'e'..='f', // A
                'g'..='h', // B
                'i'..='j', // A
                'k'..='l'  // B
            ]
        );

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_fully_nested_ranges() {
        // Test ranges where one is fully contained in another
        let set_a = vec!['a'..='z'];
        let set_b = vec!['e'..='t'];
        let set_c = vec!['j'..='o'];
        let sets = [&set_a, &set_b, &set_c];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(
            partitions,
            vec![
                'a'..='d', // only A
                'e'..='i', // A and B
                'j'..='o', // A, B, and C
                'p'..='t', // A and B
                'u'..='z'  // only A
            ]
        );

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_unicode_ranges() {
        // Test with unicode characters beyond ASCII
        let set_a = vec!['α'..='ω']; // Greek letters
        let set_b = vec!['δ'..='ψ'];
        let sets = [&set_a, &set_b];

        let partitions = collect_sorted_partitions(&sets);

        // Verify non-overlapping
        for window in partitions.windows(2) {
            assert!(*window[0].end() < *window[1].start());
        }

        // Should have 3 partitions
        assert_eq!(partitions.len(), 3);
        assert_eq!(*partitions.first().unwrap().start(), 'α');
        assert_eq!(*partitions.last().unwrap().end(), 'ω');

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_with_gaps_and_overlaps() {
        // Complex scenario with both gaps and overlaps
        let set_a = vec!['a'..='c', 'j'..='l', 's'..='u'];
        let set_b = vec!['b'..='d', 'k'..='m', 't'..='v'];
        let set_c = vec!['c'..='e', 'l'..='n', 'u'..='w'];
        let sets = [&set_a, &set_b, &set_c];

        let partitions = collect_sorted_partitions(&sets);

        // Verify all partitions are non-overlapping
        for window in partitions.windows(2) {
            assert!(*window[0].end() < *window[1].start());
        }

        // Should start at 'a' and end at 'w'
        assert_eq!(*partitions.first().unwrap().start(), 'a');
        assert_eq!(*partitions.last().unwrap().end(), 'w');

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_boundary_at_zero() {
        // Test with ranges starting at the beginning of char range
        let set_a = vec!['\0'..='\x0F'];
        let set_b = vec!['\x08'..='\x17'];
        let sets = [&set_a, &set_b];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(
            partitions,
            vec![
                '\0'..='\x07',   // only A
                '\x08'..='\x0F', // A and B
                '\x10'..='\x17'  // only B
            ]
        );

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }

    #[test]
    fn test_partition_full_unicode_range() {
        // Test spanning the entire unicode range
        let set_a = vec!['\0'..=char::MAX];
        let set_b = vec!['a'..='z'];
        let sets = [&set_a, &set_b];

        let partitions = collect_sorted_partitions(&sets);

        assert_eq!(partitions.len(), 3);
        assert_eq!(
            partitions[0],
            '\0'..=char::from_u32('a' as u32 - 1).unwrap()
        );
        assert_eq!(partitions[1], 'a'..='z');
        assert_eq!(
            partitions[2],
            char::from_u32('z' as u32 + 1).unwrap()..=char::MAX
        );

        let range_partitions = collect_range_partitions(&sets);
        assert_eq!(range_partitions, partitions);
    }
}
