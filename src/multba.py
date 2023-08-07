import pandas as pd
import numpy as np

df1 = pd.read_csv('/GD/My Drive/nasser_temp/v2_fatal-police-shootings-data.csv')
df2 = pd.read_csv('/GD/My Drive/nasser_temp/v2_fatal-police-shootings-agencies.csv')

# Assuming your DataFrame has a 'date' column, convert it to datetime objects
df1['date'] = pd.to_datetime(df1['date'])

# Create two separate DataFrames based on the date ranges
start_date_1 = pd.to_datetime('2016-01-01')
end_date_1 = pd.to_datetime('2019-06-30')
df_1 = df1[(df1['date'] >= start_date_1) & (df1['date'] <= end_date_1)]

start_date_2 = pd.to_datetime('2020-01-01')
end_date_2 = pd.to_datetime('2023-06-30')
df_2 = df1[(df1['date'] >= start_date_2) & (df1['date'] <= end_date_2)]

# Assuming your DataFrame has a 'date' column, convert it to datetime objects
df_1['date'] = pd.to_datetime(df_1['date'])

# Group by 'id' and calculate the counts for each 'id'
grouped_counts = df_1.groupby('agency_ids').size().reset_index(name='total_counts')

# Create a DataFrame table for each unique 'id' with total counts of shootings
df_table = pd.DataFrame(grouped_counts)

# Assuming your DataFrame has a 'date' column, convert it to datetime objects
df_2['date'] = pd.to_datetime(df_2['date'])

# Group by 'id' and calculate the counts for each 'id'
grouped_counts = df_2.groupby('agency_ids').size().reset_index(name='total_counts')

# Create a DataFrame table for each unique 'id' with total counts of shootings
df_table2 = pd.DataFrame(grouped_counts)

df_table.to_csv('/GD/My Drive/nasser_temp/during_covid.csv')
df_table2.to_csv('/GD/My Drive/nasser_temp/post_covid.csv')
df_1.to_csv('/GD/My Drive/nasser_temp/during_covid_master.csv')
df_2.to_csv('/GD/My Drive/nasser_temp/post_covid_master.csv')

# Assuming your DataFrame has a 'date' column, convert it to datetime objects
df_1['date'] = pd.to_datetime(df_1['date'])

# Create a new DataFrame to store the number of shootings by state
state_shootings_1 = df_1.groupby('state').size().reset_index(name='number_of_shootings')

# Calculate the yearly average shootings for each state
state_shootings_1['yearly_average_shootings'] = state_shootings_1['number_of_shootings'] / 3.5


state_shootings_1.info()
state_shootings_1.to_csv('/GD/My Drive/nasser_temp/during_covid_state.csv')


# Assuming your DataFrame has a 'date' column, convert it to datetime objects
df_2['date'] = pd.to_datetime(df_2['date'])

# Create a new DataFrame to store the number of shootings by state
state_shootings_2 = df_2.groupby('state').size().reset_index(name='number_of_shootings')

# Calculate the yearly average shootings for each state
state_shootings_2['yearly_average_shootings'] = state_shootings_2['number_of_shootings'] / 3.5


state_shootings_2.info()
state_shootings_2.to_csv('/GD/My Drive/nasser_temp/after_covid_state.csv')


# Assuming your DataFrame has a 'date' column, convert it to datetime objects
df_1['date'] = pd.to_datetime(df_1['date'])

# Create a new DataFrame to store the number of shootings by state
state_shootings_1 = df_1.groupby('state').size().reset_index(name='number_of_shootings')

# Calculate the total number of shootings
total_shootings = state_shootings_1['number_of_shootings'].sum()

# Calculate the percentage of shootings for each state
state_shootings_1['percentage_of_shootings'] = (state_shootings_1['number_of_shootings'] / total_shootings) * 100


# Assuming your DataFrame has a 'date' column, convert it to datetime objects
df_2['date'] = pd.to_datetime(df_2['date'])

# Create a new DataFrame to store the number of shootings by state
state_shootings_2 = df_2.groupby('state').size().reset_index(name='number_of_shootings')

# Calculate the total number of shootings
total_shootings = state_shootings_2['number_of_shootings'].sum()

# Calculate the percentage of shootings for each state
state_shootings_2['percentage_of_shootings'] = (state_shootings_2['number_of_shootings'] / total_shootings) * 100


# Create a figure and axis objects
fig, ax = plt.subplots(figsize=(48, 24))

# Calculate the number of states
num_states = len(state_shootings_1)

# Set the width of the bars
bar_width = 0.4

# Calculate the positions for the bars
bar_positions_1 = np.arange(num_states)
bar_positions_2 = bar_positions_1 + bar_width

# Plot the bars for during COVID
ax.bar(bar_positions_1, state_shootings_1['percentage_of_shootings'], width=bar_width, label='Before COVID')

# Plot the bars for after COVID
ax.bar(bar_positions_2, state_shootings_2['percentage_of_shootings'], width=bar_width, label='During COVID')

# Set the x-axis labels and ticks
ax.set_xticks(bar_positions_1 + bar_width / 2)
ax.set_xticklabels(state_shootings_1['state'], fontsize = 30)

# Set the y-axis ticks with bigger font size
ax.set_yticklabels(ax.get_yticks(), fontsize=30)

# Set the axis labels and title
ax.set_xlabel('State', fontsize = 56)
ax.set_ylabel('Percentage of Shootings', fontsize = 56)
ax.set_title('Percentage of Shootings During and After COVID for Each State')

# Add the legend
ax.legend(fontsize = 40)

# Show the plot
plt.tight_layout()
plt.show()