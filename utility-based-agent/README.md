# Utility-Based Agent

A **Utility-Based Agent** makes decisions by computing the utility (desirability) of outcomes and choosing actions that maximize expected utility. It quantifies how good different states are, allowing for nuanced trade-offs.

## Characteristics

- **Utility function** - Quantifies desirability of states
- **Trade-off reasoning** - Balances competing objectives
- **Optimal decisions** - Chooses action with highest utility
- **Handles preferences** - Can express degrees of preference
- **Most sophisticated** - Most advanced decision-making approach

## How It Works

```
┌─────────────┐     ┌─────────────────┐     ┌─────────────┐
│   Sensors   │────▶│ Utility         │────▶│  Actuators  │
│  (Percept)  │     │ Computation     │     │  (Action)   │
└─────────────┘     └────────┬────────┘     └─────────────┘
                             │
                    ┌────────▼────────┐
                    │ UTILITY FUNCTION│
                    │ U = comfort -   │
                    │     water_cost  │
                    └─────────────────┘
```

For each possible action, the agent computes the expected utility and selects the action with the highest value.

## Utility Function

```
Utility = Comfort - Water_Cost

where:
  Comfort    = 1 - |predicted_moisture - 0.6|
  Water_Cost = 0.1 if irrigating, else 0.0
```

### Key Innovation: Predicted State

The agent evaluates the **predicted** moisture after taking the action, not the current moisture:

```
predicted_moisture(:irrigate) = moisture + 0.25 - 0.02 = moisture + 0.23
predicted_moisture(:wait)     = moisture - 0.02
```

## Key Code Sections

### Parameters

```lisp
(defparameter *evaporation-rate* 0.02)
(defparameter *irrigation-gain* 0.25)
```

### Utility Computation

```lisp
(defun compute-utility (moisture action)
  (let* ((goal 0.6)
         (predicted-moisture
           (cond
             ((eq action :irrigate)
              (min 1.0 (+ moisture *irrigation-gain* (- *evaporation-rate*))))
             (t (max 0.0 (- moisture *evaporation-rate*)))))
         (distance-penalty (abs (- predicted-moisture goal)))
         (comfort (- 1.0 distance-penalty))
         (water-cost (if (eq action :irrigate) 0.1 0.0)))
    (- comfort water-cost)))
```

**Step by Step:**

1. Calculate predicted moisture after action
2. Compute distance from goal (0.6)
3. Convert distance to comfort (closer = higher)
4. Subtract water cost if irrigating
5. Return total utility

### Agent Decision

```lisp
(defun utility-based-agent (percept)
  (let ((moisture (getf percept :moisture))
        (raining (getf percept :raining)))
    (if raining
        :wait
        (let ((u-irrigate (compute-utility moisture :irrigate))
              (u-wait (compute-utility moisture :wait)))
          (if (> u-irrigate u-wait) :irrigate :wait)))))
```

## Utility Calculation Example

**Current moisture: 0.25**

### For :irrigate

```
predicted = min(1.0, 0.25 + 0.25 - 0.02) = 0.48
distance  = |0.48 - 0.6| = 0.12
comfort   = 1.0 - 0.12 = 0.88
cost      = 0.1
utility   = 0.88 - 0.1 = 0.78
```

### For :wait

```
predicted = max(0.0, 0.25 - 0.02) = 0.23
distance  = |0.23 - 0.6| = 0.37
comfort   = 1.0 - 0.37 = 0.63
cost      = 0.0
utility   = 0.63 - 0.0 = 0.63
```

**Decision**: 0.78 > 0.63 → **IRRIGATE**

## Utility Table

| Moisture | U(:irrigate) | U(:wait) | Action                     |
| -------- | ------------ | -------- | -------------------------- |
| 0.10     | 0.63         | 0.48     | IRRIGATE                   |
| 0.20     | 0.73         | 0.58     | IRRIGATE                   |
| 0.30     | 0.83         | 0.68     | IRRIGATE                   |
| 0.40     | 0.87         | 0.78     | IRRIGATE                   |
| **0.50** | **0.77**     | **0.88** | **WAIT** ← Crossover point |
| 0.60     | 0.67         | 0.98     | WAIT                       |
| 0.70     | 0.57         | 0.92     | WAIT                       |
| 0.80     | 0.50         | 0.82     | WAIT                       |

The crossover happens around moisture 0.44-0.45.

## Why Predicted State Matters

**Without prediction** (flawed approach):

```
U(:irrigate) = comfort(current) - 0.4
U(:wait)     = comfort(current) - 0.0
```

→ U(:wait) is ALWAYS 0.4 higher! Agent never irrigates.

**With prediction** (correct approach):

```
U(:irrigate) = comfort(predicted_after_irrigate) - 0.1
U(:wait)     = comfort(predicted_after_wait) - 0.0
```

→ Agent reasons about consequences!

## Comparison with Other Agents

| Agent Type        | Decision Basis       | Trade-offs                 |
| ----------------- | -------------------- | -------------------------- |
| Simple Reflex     | Current percept      | None                       |
| Model-Based       | Percept + History    | None                       |
| Goal-Based        | Distance to goal     | Binary (at goal or not)    |
| **Utility-Based** | **Expected utility** | **Quantified preferences** |

## Running the Simulation

```bash
cd /path/to/DCIT_313
sbcl --script simulate_all.lisp
```

## Example Output

```
=== UTILITY-BASED AGENT (12 steps) ===
Step  1 | Moisture:  0.25 | Rain: NIL | Action: IRRIGATE
Step  2 | Moisture:  0.48 | Rain: NIL | Action: WAIT
Step  3 | Moisture:  0.46 | Rain: NIL | Action: WAIT
Step  4 | Moisture:  0.44 | Rain: NIL | Action: IRRIGATE  ;; Utility crossover!
```

The agent irrigates precisely when the predicted benefit outweighs the water cost.
