# Pairing Heaps in Racket

A functional programming project implementing **pairing heaps (PH)** in Racket and applying them to various data processing tasks, such as sorting, movie ranking, and dynamic median tracking. The implementation progressively builds from the basic heap structure to more advanced abstractions involving higher-order functions, recursion schemes, and streams.

---

## Overview
The project explores **pairing heaps**—a simple, efficient, and elegant heap representation—implemented entirely in a **functional style** using Racket.  
It starts from basic heap operations, extends them with abstraction over comparison criteria, and culminates in stream-based algorithms for continuously updating medians.

**Language:** Racket  
**Focus:** Functional programming, recursion, higher-order functions, streams, and data abstraction  

---

## Features

### Core Heap Operations
- **Creation and accessors**
  - `empty-ph`, `val->ph`, `ph-empty?`, `ph-root`, `ph-subtrees`
- **Heap manipulation**
  - `merge`, `ph-insert`, `list->ph`
  - `two-pass-merge-LR`, `two-pass-merge-RL`, `tournament-merge`
  - `ph-del-root` for deleting the heap root using recursive merging

### Abstraction and Generalization
- **Order parameterization** — heaps generalized into min-heaps, max-heaps, and custom comparison heaps via `merge-f`
- **Movie structure integration**
  - Custom data type `movie` with fields for name, rating, genre, duration, and metadata
  - Functional utilities such as `mark-as-seen`, `rating-stats`, and `extract-name-rating`
- **Functional programming patterns**
  - Extensive use of *map*, *filter*, *foldr*, *foldl*, and partial application instead of explicit recursion

### Applied Algorithms
- **Movie ranking and selection**
  - Functions like `make-rating-ph`, `make-genre-ph`, `best-k-rating`, and `best-k-duration` use heaps to extract and order movies based on specific criteria
- **Dynamic rating updates**
  - Maintains real-time median updates from a continuous stream of ratings using two balanced heaps (min-heap and max-heap)
  - Stream-based functions `add-rating`, `reviews->quads`, and `quads->medians` track rating evolution efficiently

---

## Example Use Cases
- Build a **max-heap** from a list of values or movie ratings  
- Extract top *k* rated or shortest films based on heap ordering  
- Track **median ratings** dynamically from a stream of movie reviews  
- Apply abstract comparison functions to create flexible heap behaviors  

---

## Learning Outcomes
- Implemented **purely functional data structures** using recursion and list processing  
- Practiced **higher-order programming**, *currying*, and *point-free style* abstractions  
- Designed **stream-processing algorithms** without side effects or mutable state  
- Applied **functional composition and abstraction** principles to real data models (movies, ratings, preferences)  
