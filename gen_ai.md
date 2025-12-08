# Generative AI Usage

This project relies heavily on LLM Prompting to create generated ratings on Community Notes. \
All of the coding in this project was done without the use of generative AI, with the exception \
of the prompting code (found in `grok_notes_prompting.R`)

## Prompt Engineering 
As we have limited experience with prompt engineering, we concluded that using Gemini to help \
develop our system and user prompts would be really helpful. Gemini has the functionality to \
create ratings, as we did with Grok in this project, and therefore we felt that there would \
likely be a large amount of domain knowledge within it's training set. We use Grok in the \
actual project, with the assumption that X would use it's own generative model for this kind \
of work. However, we used Gemini's chat feature to develop the prompt because our institution,\
NYU, gives us access to it for free.

## httr2 Code 
Given that we have a very strict output format for our LLM prompt, we decided that we would also\
use Gemini to help debug the code for doing the actual prompting (using the httr2 package). We \
initially struggled to get the exact output we were hoping for, but Gemini was very useful \
in helpful to uncover the issues in the format of our JSON schema and the particulars of the \
request. Again, we saw Gemini as a domain expert in this, as it is used for this exact task, \
and decided to utilize it for coding the requests using the `httr2` package.

