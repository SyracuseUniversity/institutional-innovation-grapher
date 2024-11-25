import json
import csv
from datetime import datetime
from github import Github, Auth
from tqdm import tqdm

with open(".env") as envfile:
    config = json.loads(envfile.read())

auth = Auth.Token(config['githubtoken'])
g = Github(auth=auth)

repolanguagelist = []
repolicenselist = []
repostargazerlist = []
repowatcherlist = []
repoforklist = []

finalgithubrepodetailscsvrows = []
githubrepodetailscsvcolumns = ['name','full_name','html_url','description','fork','created_at','updated_at','size','stargazers_count','watchers_count','language','forks_count','archived','open_issues_count','allow_forking','topics','forks','visibility','open_issues']

username_file = open("github_usernames.txt", "r")
for username in username_file:
    username = username.strip()
    print("Getting repos for %s..." % username)
    try:
        user = g.get_user(username)
    except:
        print("User %s does not exist" % username)
        continue
    repos = user.get_repos()
    for i in tqdm(range(repos.totalCount)):
        repo = repos[i]
        repocsvrow = []
        repocsvrow.append(username)
        for att in githubrepodetailscsvcolumns:
            tmp = getattr(repo, att)
            if att == 'description' and tmp is not None:
                tmp = tmp.encode('ascii', 'ignore').decode()
            repocsvrow.append(tmp)
        repocsvrow.append(getattr(repo.license, 'name', 'None'))
        repocsvrow.append(json.dumps(repo.get_languages()))
        finalgithubrepodetailscsvrows.append(repocsvrow)

githubrepodetailscsvcolumns.insert(0, 'owner')
githubrepodetailscsvcolumns.extend(['license', 'language_lines'])

with open("outputs/repos_%s.csv" % datetime.now().strftime("%Y-%m-%d"), "w", newline="") as opencsv:
    csvwriter = csv.writer(opencsv)
    csvwriter.writerow(githubrepodetailscsvcolumns)
    for i, csvrow in enumerate(finalgithubrepodetailscsvrows):
        csvwriter.writerow(csvrow)
    
username_file.close()