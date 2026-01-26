# SI.lisp — Smart Irrigation (Core Overview) 🔧

This document explains the core code blocks in `SI.lisp` — a compact model-based reflex agent for a smart irrigation system written in Common Lisp.

## 1. Data Structures

- `agent` (defined with `defstruct`) holds the agent's internal model and configuration:
  - `soil-moisture` — current internal estimate (0..100)
  - `valve-on` — symbol (`OPEN` or `CLOSED`) representing the valve state
  - `last-watering-time`, `last-rain-time` — timestamps
  - `moisture-low`, `moisture-high` — thresholds for starting/stopping irrigation
  - `irrigation-rate`, `evaporation-rate` — per-step change rates
  - `cooldown` — minimum steps between waterings

- `percept` (also `defstruct`) is the sensor input for each timestep:
  - `soil-moisture`, `raining`, `time`

## 2. Model Update: `update-model`

Signature: `(update-model agent percept)`

- Updates the agent's internal `soil-moisture` using the percept
- Records `last-rain-time` when the percept indicates rain
- Returns the updated `agent` object

## 3. Decision Rule: `decide-action`

Signature: `(decide-action agent percept)`

- Reads internal state (moisture, cooldown, last watering) and percepts (raining, time).
- Applies rules, returning one of the symbols: `'open-valve`, `'close-valve`, or `'no-op`.
- Important detail: the implementation now checks the current valve state and avoids returning actions that would not change it. For example, if the valve is already `'CLOSED`, a rain percept will result in `'no-op` instead of repeated `'close-valve`.
- Rules summary:
  - **Open valve** if moisture < `moisture-low`, not raining, cooldown passed, and valve not already `'OPEN`.
  - **Close valve** if raining or moisture >= `moisture-high`, and valve not already `'CLOSED`.
  - **No-op** otherwise (including when the desired action would be redundant)

## 4. Actuator: `execute-action`

Signature: `(execute-action agent action now)`

- If `action` is `open-valve`, sets `(agent-valve-on agent)` to the symbol `'OPEN` and updates `last-watering-time`.
- If `action` is `close-valve`, sets `(agent-valve-on agent)` to the symbol `'CLOSED`.
- `no-op` leaves the agent unchanged and returns `nil`.
- Because `decide-action` avoids redundant actions, repeated `close-valve`/`open-valve` calls are avoided; the actuator behavior remains simple and idempotent.

## 5. Environment Simulation: `environment-step`

Signature: `(environment-step current-moisture agent percept)`

- Computes moisture change for one timestep:
  - Increase from rain (`raining` percept)
  - Increase from irrigation when valve is `'OPEN` (uses `irrigation-rate` and checks with `eq`)
  - Decrease due to evaporation (`evaporation-rate`)
- Clamps moisture to the range 0..100 and returns the new moisture value

## 6. Tests & Simulation

- Unit tests: `test_SI.lisp` contains a small test runner covering decision logic, actuator behavior, and environment-step. Run with:
  - `sbcl --script test_SI.lisp`
- Simulation runner: `sim_SI.lisp` provides `run-sim` and auto-runs when invoked as a script. Examples:
  - `sbcl --script sim_SI.lisp` (auto-run)
  - `sbcl --script sim_SI.lisp -- --run-sim` (explicit flag)
