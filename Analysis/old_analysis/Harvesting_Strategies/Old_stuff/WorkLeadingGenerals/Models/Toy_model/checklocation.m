function [food] = checklocation(Li,world)
% Details: this function gets an X,Y coordinates, if the X,Y coordinates
% have food on them (i.e. = 1), then function returns a 1, else a 0. 
if world(Li(2),Li(1)) > 0   % I think world is plotted oppositely in pcolor
    food = 1;
else food = 0;
end

    
