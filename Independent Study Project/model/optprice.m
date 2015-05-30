function p0 = optprice(gamma, c, mu, sigma)
% this function calculates the price of a bond that pays C always in full


p0 = c .* exp( -gamma .* mu  + (1/2) .* gamma.^2 * sigma.^2 );

end