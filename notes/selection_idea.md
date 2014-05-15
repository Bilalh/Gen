

##  Using only compact first 

Given:	time per model t_p

Aim:	To find some instances that don't timeout. Ideally we want a range of instances in the range [1,t_p] which don't time timeout

Why:	Say we have 1000 models where most instances timeout it.  Since compact is probly  reasonable model, we randomly generated some instances until we have k instances that don't timeout. 

Use:	We have some instances that don't timeout, we then run of the rest of the model and continue using our methods, this would have same hours because of not having to wait to all models to timeout. 



##  When Conjure Generates too many models 

Why:	When Conjure generates too many models, e.g. vessel loading > 20000 models after ~10 days with no channelling, We need a different method of selecting  a set of good models.  

Aim:    To find models better then compact. Also find a model for each fracture. The model generalises

How:	Start with compact since it a reasonable model, generate k random models and run the method on it.  After 