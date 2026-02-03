# Smart Irrigation Goal-Based Agent

## Description

This is a Lisp program that simulates a goal-based agent for smart irrigation. The agent monitors soil moisture levels and automatically adjusts irrigation to maintain optimal moisture levels between 40% and 60%. The program demonstrates basic AI concepts in a practical application.

## Features

- **Soil Moisture Monitoring**: Continuously checks current soil moisture levels
- **Automatic Irrigation Control**: Turns irrigation on when moisture falls below 40% and off when it exceeds 60%
- **Simulation**: Simulates moisture increase when irrigation is active
- **Goal-Based Decision Making**: Uses conditional logic to achieve and maintain optimal soil conditions
- **Continuous Operation**: Runs in a loop until optimal moisture is achieved

## Requirements

- A Common Lisp implementation such as:
  - SBCL (Steel Bank Common Lisp)
  - CLISP
  - CCL (Clozure Common Lisp)

## How to Run

1. Ensure you have a Common Lisp environment installed
2. Load the `smart_irrigation.lisp` file into your Lisp REPL or IDE
3. Evaluate the entire file or run `(start-irrigation-system)` to begin the simulation

The program will automatically start monitoring and adjusting irrigation based on the simulated soil moisture levels.

## Code Structure

- `*optimal-moisture-min*` & `*optimal-moisture-max*`: Define the target moisture range
- `*soil-moisture*`: Current simulated moisture level (starts at 35%)
- `turn-on-irrigation()`: Activates irrigation and increases moisture by 10%
- `turn-off-irrigation()`: Deactivates irrigation
- `irrigation-agent()`: Core decision-making function that checks conditions and takes actions
- `start-irrigation-system()`: Main loop that runs the agent until optimal conditions are met

## Example Output

```
Checking soil moisture... Current: 35%
Irrigation turned ON.
Soil moisture increased to 45%.
Checking soil moisture... Current: 45%
Soil moisture is optimal (45%). No action needed.
Goal achieved: Soil moisture is optimal (45%). System stopping.
```

## Notes

- This is a simulation for educational purposes
- The sleep function introduces a 2-second delay between checks for demonstration
- Moisture levels are simulated and not based on real sensor data
