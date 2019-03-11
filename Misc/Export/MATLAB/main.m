function main

tMax = 30.0;   % Simulation duration (s)

% Initialise the model(taken from test.m)

Y = [0.0, 0.325, 0.6, 0.05];

% Compute the model

[tData, YData] = ode15s(@test, [0 tMax], Y);

% Plot the results

plot(tData, YData);
