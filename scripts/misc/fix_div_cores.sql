-- Fixes the times for the `div_cores` directory of results

UPDATE ksample
SET    total_timeout  = (total_timeout/32), 
       models_timeout = (models_timeout/32)
WHERE  essence        = 'prob024-Langford';

UPDATE uniform
SET    total_timeout  = (total_timeout/32), 
       models_timeout = (models_timeout/32)
WHERE  essence        = 'prob024-Langford';


UPDATE ksample
SET    total_timeout  = (total_timeout/4), 
       models_timeout = (models_timeout/4)
WHERE  essence        = 'prob010-SCP';

UPDATE markov
SET    total_timeout  = (total_timeout/4), 
       models_timeout = (models_timeout/4)
WHERE  essence        = 'prob010-SCP';

UPDATE nsample
SET    total_timeout  = (total_timeout/4), 
       models_timeout = (models_timeout/4)
WHERE  essence        = 'prob010-SCP';

UPDATE smac
SET    total_timeout  = (total_timeout/4), 
       models_timeout = (models_timeout/4)
WHERE  essence        = 'prob010-SCP';

UPDATE uniform
SET    total_timeout  = (total_timeout/4), 
       models_timeout = (models_timeout/4)
WHERE  essence        = 'prob010-SCP';