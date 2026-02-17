# Model-Based Reflex Agent

A **Model-Based Reflex Agent** extends the simple reflex agent by maintaining internal state (memory) to track aspects of the world that cannot be directly perceived.

## Characteristics

- **Has memory** - Maintains internal state across decisions
- **Tracks history** - Remembers past actions and events
- **Informed decisions** - Uses both percept AND internal model
- **Handles partial observability** - Can infer hidden state
- **More sophisticated** - Than simple reflex agents

## How It Works

```
┌─────────────┐     ┌─────────────────┐     ┌─────────────┐
│   Sensors   │────▶│  Internal Model │────▶│  Actuators  │
│  (Percept)  │     │  + IF-THEN Rules│     │  (Action)   │
└─────────────┘     └────────┬────────┘     └─────────────┘
                             │
                    ┌────────▼────────┐
                    │  Internal State │
                    │  (Memory)       │
                    └─────────────────┘
```

The agent maintains a model of the world that gets updated with each percept, allowing it to make decisions based on both current observations AND history.

## Internal State

```lisp
(:last-rain <boolean> :last-action <keyword>)
```

- `:last-rain` - Whether it rained in the previous time step
- `:last-action` - The action taken in the previous time step

## Decision Rules

| Rule | Condition                                  | Action   | Reasoning                 |
| ---- | ------------------------------------------ | -------- | ------------------------- |
| 1    | Raining                                    | WAIT     | Don't waste water         |
| 2    | Moisture < 0.35                            | IRRIGATE | Soil is very dry          |
| 3    | Last action = IRRIGATE AND moisture < 0.45 | IRRIGATE | Continue irrigation cycle |
| 4    | Default                                    | WAIT     | Conservative default      |

**Rule 3 is the key difference** - it uses memory to continue an irrigation cycle once started.

## Key Code Sections

### Agent Factory (Closure Pattern)

```lisp
(defun make-model-based-reflex-agent ()
  (let ((state (list :last-rain nil :last-action nil)))  ;; Internal state
    (lambda (percept)
      ;; Agent logic here - can access and modify 'state'
      ...)))
```

**Key Points:**

- Uses a **closure** to encapsulate internal state
- `state` persists between function calls
- Each call to `make-model-based-reflex-agent` creates a new agent with fresh state

### Decision Logic with Memory

```lisp
(action
  (cond
    (raining :wait)                           ;; Rule 1
    ((< moisture 0.35) :irrigate)             ;; Rule 2
    ((and (getf state :last-action)           ;; Rule 3: Check memory!
          (eq (getf state :last-action) :irrigate)
          (< moisture 0.45)) :irrigate)
    (t :wait)))                               ;; Rule 4
```

### State Update

```lisp
;; After deciding action, update the internal model
(setf (getf state :last-rain) raining)
(setf (getf state :last-action) action)
```

## Advantage Over Simple Reflex

Consider this scenario:

| Step | Moisture | Simple Reflex | Model-Based           |
| ---- | -------- | ------------- | --------------------- |
| 1    | 0.30     | IRRIGATE      | IRRIGATE              |
| 2    | 0.43     | WAIT          | IRRIGATE (continues!) |
| 3    | 0.46     | WAIT          | WAIT                  |

The model-based agent **continues irrigating** until moisture reaches 0.45, preventing the "start-stop" behavior of the simple reflex agent.

## Closure Explanation

```lisp
;; Creating the agent
(let ((agent (make-model-based-reflex-agent)))
  ;; Using the agent
  (funcall agent '(:moisture 0.3 :raining nil))  ;; Returns :irrigate
  (funcall agent '(:moisture 0.4 :raining nil))) ;; Returns :irrigate (remembers!)
```

The `let` inside `make-model-based-reflex-agent` creates a private `state` variable that only the returned lambda can access - this is the "closure" that gives the agent memory.

## Running the Simulation

```bash
cd /path/to/DCIT_313
sbcl --script simulate_all.lisp
```

## Example Output

```
Step  1 | Moisture:  0.25 | Rain: NIL | Action: IRRIGATE
Step  2 | Moisture:  0.48 | Rain: NIL | Action: WAIT
```

The agent uses its memory of the last action to make more informed decisions.
