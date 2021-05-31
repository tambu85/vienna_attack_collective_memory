#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: marcella tambuscio
"""
import sys
import tagme
import requests
import pandas as pd
import numpy as np
from urllib import error
from SPARQLWrapper import SPARQLWrapper, JSON
from SPARQLWrapper.SPARQLExceptions import EndPointNotFound, EndPointInternalError,SPARQLWrapperException
import time


pd.options.mode.chained_assignment = None


def get_coordinates(id):
    url = "https://en.wikipedia.org/w/api.php?action=query&prop=coordinates&format=json&pageids="+str(id)
    r = requests.get(url)
    json_data = r.json()
    path=json_data['query']['pages'][str(id)]
    if('coordinates' in path):
        coord= path['coordinates']
    else:
        coord=0
    return(coord)
    
def get_sparql_results(endpoint_url, query):
    user_agent = "WDQS-example Python/%s.%s" % (sys.version_info[0], sys.version_info[1])
    # TODO adjust user agent; see https://w.wiki/CX6
    endpoint_sparql = SPARQLWrapper(endpoint_url, agent=user_agent)
    endpoint_sparql.setQuery(query)
    endpoint_sparql.setReturnFormat(JSON)
    
    result = None
    try:
      result = endpoint_sparql.query().convert()
    except(error.HTTPError,EndPointInternalError,EndPointNotFound,SPARQLWrapperException) as e: 
        print("error")
        print(e)
        time.sleep(10)
   
    return(result)

    
def is_location(id):
    endpoint_url = "https://query.wikidata.org/sparql"
    query = """SELECT ?item ?is_geo WHERE {
      SERVICE wikibase:mwapi {
        bd:serviceParam wikibase:endpoint "en.wikipedia.org" .
        bd:serviceParam wikibase:api "Generator" .
        bd:serviceParam mwapi:generator "revisions" .
        bd:serviceParam mwapi:pageids \""""+str(id)+"""\".
        ?item wikibase:apiOutputItem mwapi:item .
      }
      BIND ( EXISTS {?item wdt:P31/wdt:P279+ wd:Q618123} AS ?is_geo )
    }
    """
    res = get_sparql_results(endpoint_url, query)

    bool_res=False
    if(res!=None):
        if(res["results"]["bindings"]!=[]):
            res = res["results"]["bindings"][0]['is_geo']['value']
            if(res=='true'):
                bool_res=True

    return(bool_res)


orig_stdout = sys.stdout
access_token="insertyouraccesstokenTAGME"
modif_tagme.GCUBE_TOKEN = access_token

locations = pd.read_csv ('/vienna0211/data/locations.csv',names=["idx","raw"], header=None)
locations = locations.drop(locations.columns[0], axis=1)
locations['clean']=""
locations['lon']=np.nan
locations['lat']=np.nan

for i in range(0,len(locations)):
    
    l=locations['raw'][i]
    locations['clean'][i]=""
    locations['lon'][i]= np.nan
    locations['lat'][i]= np.nan
    
    if(l is not np.nan):
        ent = tagme.annotate(l)
        ent_dict=ent.original_json['annotations']    
        
    
        if(len(ent_dict)>0):
            e = ent_dict[0]   
                
            if('title' in e.keys() and e['rho']>0.05 and e['link_probability']>0.05):
        
               ent_name = e['title']    
               wid = e['id']
               
        
               y=is_location(wid)
               if is_location(wid) :
                   print(ent_name)
                   locations['clean'][i]=ent_name
                   coord = get_coordinates(wid)
                   if(coord!=0):
                       locations['lon'][i]= coord[0]['lon']
                       locations['lat'][i]= coord[0]['lat']
         

   
        
output = open('/vienna0211/data/locations_clean.csv','w+')
locations.to_csv(output, header=True)
        
