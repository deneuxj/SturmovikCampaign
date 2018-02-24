#!/bin/bash

rm -rf SturmovikCampaign-v
mkdir SturmovikCampaign-v

mkdir -p SturmovikCampaign-v/Campaign/
cp -r Campaign/bin/Release SturmovikCampaign-v/Campaign/bin
mkdir -p SturmovikCampaign-v/CampaignControlApp/
cp -r CampaignControlApp/bin/Release SturmovikCampaign-v/CampaignControlApp/bin
