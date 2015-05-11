classdef Country
    properties (Access = public)
        
        Gamma; % risk aversion coefficient
        C; % maximum value of payoff x1

        Ghigh; % high output, when debt is pay in full
        Glow; % low output when debt is not paid

        % these two can be obtained from our dataset
        Sigma; % std. dev. of output
        Mu; % average output
    end
    
    methods
        
        function r = price(obj)
            r = price(obj.Ghigh, obj.Glow, obj.Gamma, obj.C, obj.Mu, obj.Sigma);
        end
        function r = optprice(obj)
            r = optprice(obj.Gamma, obj.C, obj.Mu, obj.Sigma);
        end
        function r = haircut(obj)
            r = haircut(price(obj.Ghigh, obj.Glow, obj.Gamma, obj.C, obj.Mu, obj.Sigma) ...
                ,optprice(obj.Gamma, obj.C, obj.Mu, obj.Sigma));
        end  
    end
end