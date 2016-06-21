function [food] = checklocation(Li,world)
 
if world(Li(2),Li(1)) > 0
    food = 1; 
else food = 0;
end
    
end