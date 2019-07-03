import csv
import math
from datetime import date

THRESHOLD = 1.5

def within_num_days(date1, date2, num_days):
	# print(date1)
	month1, day1, year1 = date1.split('/')
	month2, day2, year2 = date2.split('/')
	# print(int(year1), int(month1), int(day1))
	d1 = date(int(year1), int(month1), int(day1))
	d2 = date(int(year2), int(month2), int(day2))

	diff = d2 - d1
	if (abs(diff.days) <= num_days):
		return True
	else:
		return False

with open('output.csv', 'w') as outFile:
	fileWriter = csv.writer(outFile)
	with open('labs.csv', 'r') as inFile:
		fileReader = csv.reader(inFile)

		

		rows = iter(fileReader)
		next(rows) # skips header row

		patient_id = 0
		num_consec_above = 0
		num_episodes = 0
		first_episode_dates = None

		consecutive = False
		day_is_positive = False
		in_episode = False
		dates = None


		episodes = []
		episode_len = 0
		max_value = 0
		episode_start_dates = None
		days_since_treatment = 0

		for row in rows:
			if (patient_id != 0 and patient_id != int(row[0])): # write patient output since we have new patient
				if (in_episode):
					in_episode = False
					episodes.append((episode_len, episode_start_dates, days_since_treatment, max_value))
					episode_len = 0
				output_row = [patient_id, num_episodes, episodes]
				fileWriter.writerow(output_row)
				num_consec_above = 0
				num_episodes = 0
				consecutive = False
				dates = None
				episodes = []
				episode_len = 0
				max_value = 0

				episode_start_dates = None
				days_since_treatment = 0

			patient_id = int(row[0])
			if (row[24] == "NA"):
				result = 0.0
			else:
				result = float(row[24])
			if (row[24] == "NA"):
				result2 = 0.0
			else:
				result2 = float(row[8])
			if (row[3] == dates):
				consecutive = True
			elif (dates == None):
				day_is_positive = False
			elif (row[3] != dates and within_num_days(row[3], dates, 1)):
				consecutive = True
				# if (day_is_positive):
				# 	in_episode = False
				# 	episodes.append((episode_len, episode_start_dates, days_since_treatment))
				day_is_positive = False
			else:
				day_is_positive = False
				# if (in_episode):
				# 	in_episode = False
				# 	episodes.append((episode_len, episode_start_dates, days_since_treatment))
				# 	episode_len = 0
				# 	max_value = 0
			dates = row[3]

			if (result > THRESHOLD and not day_is_positive):
				day_is_positive = True
				if (not in_episode):
					num_episodes += 1
					in_episode = True
					episode_start_dates = dates
					episode_len = 1
					day_is_positive = True
					# month1, day1, year1 = dates.split('/')
					# month2, day2, year2 = episode_start_dates.split('/')
					# d1 = date(int(year1), int(month1), int(day1))
					# d2 = date(int(year2), int(month2), int(day2))
					# diff = d2 - d1 + 1
					# episode_len = abs(diff.days)
					if (result2 > max_value):
						max_value = result2
					
					days_since_treatment = row[22]
				elif (in_episode):
					# episode_len += 1
					day_is_positive = True
					month1, day1, year1 = dates.split('/')
					month2, day2, year2 = episode_start_dates.split('/')
					d1 = date(int(year1), int(month1), int(day1))
					d2 = date(int(year2), int(month2), int(day2))
					diff = d2 - d1
					episode_len = abs(diff.days) + 1
					if (result2 > max_value):
						max_value = result2
			elif (not day_is_positive):
				if (in_episode):
					in_episode = False
					episodes.append((episode_len, episode_start_dates, days_since_treatment, max_value))
					episode_len = 0
					max_value = 0
					





		output_row = [patient_id, num_episodes, episodes]
		fileWriter.writerow(output_row)
















