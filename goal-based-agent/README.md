# Goal-Based Agent

A **Goal-Based Agent** makes decisions by considering explicit goals. It evaluates whether an action will help achieve the desired goal state, rather than just reacting to the current situation.

## Characteristics

- **Explicit goals** - Has defined objectives to achieve
- **Forward thinking** - Considers how actions affect goal achievement
- **Flexible** - Goals can be changed without rewriting rules
- **Purposeful** - Actions are chosen to reach goal states
- **Adaptable** - Same agent structure works with different goals

## How It Works

```
┌─────────────┐     ┌─────────────────┐     ┌─────────────┐
│   Sensors   │────▶│  Goal Checking  │────▶│  Actuators  │
│  (Percept)  │     │  & Planning     │     │  (Action)   │
└─────────────┘     └────────┬────────┘     └─────────────┘
                             │
                    ┌────────▼────────┐
                    │      GOAL       │
                    │ moisture ∈      │
                    │ [0.45, 0.75]    │
                    └─────────────────┘
```

The agent compares the current state to the goal state and chooses actions that move toward the goal.

## Goal Definition

```
Goal: Keep soil moisture within [goal-min, goal-max]
Default: [0.45, 0.75]
```

This is fundamentally different from reflex agents which have thresholds embedded in rules.

## Decision Logic

| Current State | Relation to Goal | Action | Reasoning |
|---------------|------------------|--------|-----------|
| Raining | - | WAIT | Rain helps achieve goal for free |
| moisture < goal-min | Below goal | IRRIGATE | Need to reach goal |
| moisture > goal-max | Above goal | WAIT | Wait for evaporation |
| goal-min ≤ moisture ≤ goal-max | At goal | WAIT | Goal achieved! |

## Key Code Sections

### Agent Factory with Parameters

```lisp
(defun make-goal-based-agent (goal-min goal-max)
  (lambda (percept)
    (let ((moisture (getf percept :moisture))
          (raining (getf percept :raining)))
      (cond
        (raining :wait)
        ((< moisture goal-min) :irrigate)  ;; Below goal
        ((> moisture goal-max) :wait)       ;; Above goal
        (t :wait)))))                       ;; At goal
```

**Key Points:**
- `goal-min` and `goal-max` are parameters, not hardcoded
- The same function creates agents with different goals
- Returns a closure that "remembers" its specific goal

### Creating Agents with Different Goals

```lisp
;; Conservative agent - narrow range
(let ((conservative (make-goal-based-agent 0.50 0.60)))
  ...)

;; Relaxed agent - wide range  
(let ((relaxed (make-goal-based-agent 0.40 0.80)))
  ...)

;; Dry-tolerant agent
(let ((dry-tolerant (make-goal-based-agent 0.30 0.70)))
  ...)
```

## Goal-Based vs Reflex Comparison

### Reflex Agent (rules embedded):
```lisp
((< moisture 0.3) :irrigate)  ;; Hardcoded threshold
```

### Goal-Based Agent (parameterized):
```lisp
((< moisture goal-min) :irrigate)  ;; Configurable goal
```

**Advantage**: Change goals without changing code!

## Goal State Visualization

```
Moisture Scale:
0.0 ─────────────────────────────────────────── 1.0
         │          GOAL RANGE          │
         │◄─────────────────────────────►│
      goal-min                       goal-max
       (0.45)                         (0.75)
         
Below goal-min → IRRIGATE
Above goal-max → WAIT
In range → WAIT (goal achieved!)
```

## Flexibility Demonstration

The same agent structure handles different scenarios:

| Scenario | goal-min | goal-max | Use Case |
|----------|----------|----------|----------|
| Water-saving | 0.40 | 0.65 | Drought conditions |
| Optimal growth | 0.50 | 0.70 | Normal conditions |
| Wet-tolerant plants | 0.55 | 0.85 | Water-loving crops |

## Running the Simulation

```bash
cd /path/to/DCIT_313
sbcl --script simulate_all.lisp
```

The simulation uses `(make-goal-based-agent 0.45 0.75)` by default.

## Example Output

```
=== GOAL-BASED AGENT (goal: 0.45-0.75) (12 steps) ===
Step  1 | Moisture:  0.25 | Rain: NIL | Action: IRRIGATE
Step  2 | Moisture:  0.48 | Rain: NIL | Action: WAIT
Step  3 | Moisture:  0.46 | Rain: NIL | Action: WAIT
Step  4 | Moisture:  0.44 | Rain: NIL | Action: IRRIGATE  ;; Dropped below 0.45!
```

Once moisture enters the goal range [0.45, 0.75], the agent waits. It only acts when outside the goal.
