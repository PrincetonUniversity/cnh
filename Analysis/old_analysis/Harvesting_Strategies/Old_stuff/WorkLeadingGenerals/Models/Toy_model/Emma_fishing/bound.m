function[newx, newy] = bound(boundary, oldx, oldy, worldsize)

if strcmp(boundary,'periodic')==1

        if oldx >= worldsize % periodic boundary conditions
            newx = 1;
        elseif oldx < 1
             newx = worldsize;
        else newx = oldx;
        end
        
        if oldy >= worldsize
            newy = 1;
        elseif oldy < 1
            newy = worldsize;
        else newy = oldy;
        end
        
end
        
end