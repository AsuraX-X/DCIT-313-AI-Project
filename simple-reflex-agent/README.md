# Simple Reflex Agent

A **Simple Reflex Agent** is the most basic type of intelligent agent. It uses condition-action rules (IF-THEN rules) that map directly from the current percept to an action.

## Characteristics

- **No memory** - Decisions based ONLY on current percept
- **No internal state** - Cannot track history
- **Simple IF-THEN rules** - Fast and lightweight
- **Reactive** - Responds immediately to stimuli
- **Best for** - Fully observable environments

## How It Works

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Sensors   │────▶│  IF-THEN    │────▶│  Actuators  │
│  (Percept)  │     │   Rules     │     │  (Action)   │
└─────────────┘     └─────────────┘     └─────────────┘
```

The agent receives a percept and immediately selects an action based on predefined rules. There is no reasoning about the past or future.

## Decision Rules

| Rule | Condition | Action | Reasoning |
|------|-----------|--------|-----------|
| 1 | Raining | WAIT | Don't waste water |
| 2 | Moisture ≥ 0.5 | WAIT | Soil is moist enough |
| 3 | Moisture < 0.3 | IRRIGATE | Soil is too dry |
| 4 | Default | WAIT | Conservative default |

## Key Code Sections

### The Agent Function

```lisp
(defun simple-reflex-agent (percept)
  (let ((moisture (getf percept :moisture))
        (raining (getf percept :raining)))
    (cond
      (raining :wait)                    ;; Rule 1
      ((>= moisture 0.5) :wait)          ;; Rule 2
      ((< moisture 0.3) :irrigate)       ;; Rule 3
      (t :wait))))                       ;; Rule 4 (default)
```

**Key Points:**
- `percept` is a property list: `(:moisture 0.4 :raining nil)`
- `getf` extracts values from the property list
- `cond` evaluates rules in order, returns first match
- Returns either `:irrigate` or `:wait`

### Percept Structure

```lisp
(:moisture <float 0.0-1.0> :raining <boolean>)
```

- `:moisture` - Soil moisture level (0.0 = bone dry, 1.0 = saturated)
- `:raining` - Whether it's currently raining (T or NIL)

## Limitations

1. **No learning** - Cannot improve over time
2. **No memory** - Forgets everything between decisions
3. **Rigid rules** - Cannot adapt to changing conditions
4. **Gap problem** - Notice moisture 0.3-0.5 range has no irrigation (the "wait gap")

## Running the Simulation

```bash
cd /path/to/DCIT_313
sbcl --script simulate_all.lisp
```

## Example Output

```
Step  1 | Moisture:  0.25 | Rain: NIL | Action: IRRIGATE
Step  2 | Moisture:  0.48 | Rain: NIL | Action: WAIT
...
Step 11 | Moisture:  0.30 | Rain: NIL | Action: IRRIGATE
```

The agent only irrigates when moisture drops below 0.3, regardless of trends.
