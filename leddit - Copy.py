# -*- coding: utf-8 -*-
"""
Created on Tue Aug 13 15:31:19 2019
Python 3.6
@author: mcmic
"""

import praw
import pandas as pd
import datetime as dt

reddit = praw.Reddit(client_id='redacted',
                     client_secret='redacted',
                     user_agent='2bdl',
                     username='redacted',
                     password='redacted')

subreddit = reddit.subreddit('2b2t')

top_subreddit = subreddit.new(limit=1000)

topics_dict = { "title":[],
                "score":[],
                "id":[],
                "url":[], 
                "comms_num": [],
                "created": [],
                "body":[]}

for submission in top_subreddit:
    topics_dict["title"].append(submission.title)
    topics_dict["score"].append(submission.score)
    topics_dict["id"].append(submission.id)
    topics_dict["url"].append(submission.url)
    topics_dict["comms_num"].append(submission.num_comments)
    topics_dict["created"].append(submission.created)
    topics_dict["body"].append(submission.selftext)

topics_data = pd.DataFrame(topics_dict)

def get_date(created):
    return dt.datetime.fromtimestamp(created)+dt.timedelta(hours=-10)

def get_day(created):
    iso=(dt.datetime.fromtimestamp(created)+dt.timedelta(hours=-10)).isoformat()
    return iso[0:4]+iso[5:7]+iso[8:10]

def get_time(created):
    iso=(dt.datetime.fromtimestamp(created)+dt.timedelta(hours=-10)).isoformat()
    return iso[11:13]+iso[14:16]+iso[17:19]

_timestamp = topics_data["created"].apply(get_date)
_day = topics_data["created"].apply(get_day)
_time = topics_data["created"].apply(get_time)

topics_data = topics_data.assign(timestamp = _timestamp)
topics_data = topics_data.assign(day = _day)
topics_data = topics_data.assign(time = _time)

topics_data.to_csv('redditout.csv', index=False) 





