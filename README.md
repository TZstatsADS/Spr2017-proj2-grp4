# Project 2: Open Data App - an RShiny app development project


In this second project of GU4243/GR5243 Applied Data Science, we develop an *Exploratory Data Analysis and Visualization* shiny app on a topic of your choice using U.S. government open data released on the [data.gov](https://data.gov/) website. See [Project 2 Description](project2_desc.md) for more details.  

The **learning goals** for this project is:

- business intelligence for data science
- data cleaning
- data visualization
- systems development/design life cycle
- shiny app/shiny server

*The above general statement about project 2 can be removed once you are finished with your project. It is optional.

## Project Title: an RShiny app development project (NY School Hunter)

Term: Spring 2017

+ Team #4
+ **Team members**:
	+ 1. Ka Heng (Helen) Lo - presenter
	+ 2. Boxuan Zhao
	+ 3. Zijun Nie
	+ 4. Senyao Han
	+ 5. Song Wang

+ **Project summary**: Our project takes all available data on colleges and universities in New York State and attempts to create a useful shiny app that allows users to explore and compare schools based on user-specific filtering criteria. The purpose of our design is to provide users with a bird's eye view of New York colleges and universities; allow them to filter, search, and group schools by their preferred criteria; and further compare two schools on a more micro level. A distinguishing feature of our app is the map search function - users can see all the specified schools on the map (normal map view or satellite map view), focus in on a specific area of New York State, and choose to group schools on an overlayed hovering map (i.e. a map overlayed on top the main map) by clusters based on their choosing (e.g. part-time or full-time programs). We made our map so detailed that users can zoom in on a single school, say on the satellite map view, and see layout of the campus along with names of buildings on the campus. As for our side-by-side comparison feature, currently it compares only two schools side-by-side, but it can easily be changed to a multi-select option for comparing more than two schools (we leave it be for this prototype of our app). The side-by-side school comparison feature allows users to see a detailed breakdown of meaningful data and statistics from our available data on each school. We've picked out and highlighted certain details that we think would be helpful to users searching for schools - e.g. tuition fees, locale, demographics, etc. Though our data on schools is limited, we extracted as much meaningful data as we could and analyzed and displayed it in an appealing and useful way, with visual aids. 

+ **Contribution statement**: ([default](doc/a_note_on_contributions.md))  
In brainstorming the interface and purpose of the app, all team members were present and engaged. However, it must be noted that as the process of developing the app progressed, team member 5 became increasingly more inactive. While team member 5's efforts in his allocated part of the project should be recognized, ultimately, they were fruitless. Further, with the lack of a tangible product, team member 5 seemed to retreat away from the rest of the project and, in effect, seemed to choose to remain in the dark about the progression and development of the app as a whole.  

Members 2 & 4 developed the base model for the map, while members 1 & 3 developed the base design for the school comparison feature. Additionally, members 1 & 3 explored specific functionalities of the map for errors, and member 1 proposed changes to the overall aesthetic/design and suggested new functionalities to add in order to improve the user-interface and make for a more user-friendly experience. Team members 2 & 4 were flexible and receptive to making the necessary changes. Generally, members 1,2,3,&4 were communicative and responsive about any minor or major improvements to the app during the entire process. All four active members were very good about working together and being involved and informed about all parts of the app at every stage of development.  

Team members 1,2,3,&4 contributed to the GitHub repository and team member 1 prepared the presentation. Team members 1,2,3,&4 approve our work presented in this GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is organized as follows.

```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```

Please see each subfolder for a README file.

