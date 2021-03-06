%% Model Calibration
% Independent Study Project

% Calibration of the model used in CEPR (2015) to estimate the size of a
% haircut. 

%% Initialization
% clear workspace
clear; clc; close all

c = 1;
range = 0.05;
ghigh = [0.01, 0, -0.01, -0.03, -0.05];
% ghigh = -0.05:0.001:0.01;
% ghigh = 0.02;
glow = ghigh - range;
gamma = 10; % if 0 risk neutral. They also consider gamma = 10.
mu = 0.01;

%% Country Class

names = {'greece', 'ireland', 'italy', 'portugal', 'spain'};


% Create a country class object for each GIIP
greece = Country;
ireland = Country; 
italy = Country;
portugal = Country;
spain = Country;

     
% average growth of each country
% this data includes 1970-2007
greece.Mu = 0.04294878;
ireland.Mu = 0.06387126;
italy.Mu = 0.02182484;
portugal.Mu = 0.03644501;
spain.Mu = 0.04764302;

% std. dev. of growth in each country
% this data includes 1970-2007
greece.Sigma = 0.03425015;
ireland.Sigma = 0.04317413;
italy.Sigma = 0.02938386;
portugal.Sigma = 0.03906404;
spain.Sigma = 0.03534576;

% Set the high value of g
greece.Ghigh = ghigh;
ireland.Ghigh = ghigh;
italy.Ghigh = ghigh;
portugal.Ghigh = ghigh;
spain.Ghigh = ghigh;

% Set the high value of g
greece.Glow = glow;
ireland.Glow = glow;
italy.Glow = glow;
portugal.Glow = glow;
spain.Glow = glow;

% Set the risk aversion coefs gamma
greece.Gamma = gamma;
ireland.Gamma = gamma;
italy.Gamma = gamma;
portugal.Gamma = gamma;
spain.Gamma = gamma;

% Set the maximum value of payoff
greece.C = c;
ireland.C = c;
italy.C = c;
portugal.C = c;
spain.C = c;

% Test country
% With the same parameters as in the CEPR (2015) paper
% It works! *Everything is awesome*
test = Country;
test.Mu = mu;
test.Sigma = 0.035;
test.Ghigh = ghigh;
test.Glow = glow;
test.Gamma = gamma;
test.C = c;

test.haircut()
%% Preliminary Graphs
% To see the effect of mu on the haircut

% greece.Mu = 0.03580729:0.0001:0.04793550;
% ireland.Mu = 0.04289705:0.0001:0.05397450;
% italy.Mu = 0.01179129:0.0001:0.03802330;
% portugal.Mu = 0.01999633:0.0001:0.04386125;
% spain.Mu = 0.03936565:0.0001:0.04761670;
% 
% figure
% 
% % Greece
% subplot(3,2,1)
% plot(greece.Mu, greece.haircut())
% title('Greece')
% 
% % Ireland
% subplot(3,2,2)
% plot(ireland.Mu, ireland.haircut())
% title('Ireland')
% 
% % Italy
% subplot(3,2,3)
% plot(italy.Mu, italy.haircut())
% title('Italy')
% 
% % Portugal
% subplot(3,2,4)
% plot(portugal.Mu, portugal.haircut())
% title('Portugal')
% 
% % Spain
% subplot(3,2,5)
% plot(spain.Mu, spain.haircut())
% title('Spain')


