# Community Notes Project

This repository contains the code for our project looking into X's Community 
Notes content moderation. More information about Community Notes can be 
found [here](https://communitynotes.x.com/guide/en/about/introduction). 


Files contained in this repo: 
- 
- `additional_work` - contains additional files `mini_data.ipynb`, `test_commnotes.ipynb` and 
`unused_methods.R`. These contain work that is not relevant to the current version of this 
project, but may be useful in future iterations. 


Note: The `mini_data.ipynb` and `test_commnotes.ipynb` files are both from an 
earlier iteration of the project in which we attempted to run the Community Notes
ranking algorithm. The hope here was to intergrate LLM generated notes into this.


The main issue we ran into was the need to alter all the tensor movements from GPU to CPU. 
Debugging this problem became too onerous for the scope of this project. However the files
are included. 

The `mini_data.ipynb` file should be within the `.data` directory, which in turn should be in 
`communitynotes/scoring/src` of a cloned https://github.com/twitter/communitynotes repo. Our attempt 
at running the code, `test_commnotes.ipynb` should also be placed in the `src` file, as it mirrors 
the `main.py` file in the same directory. 

