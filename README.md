# aitag

aitag is an R package that provides a convenient way to annotate text data using OpenAI's GPT models. It is designed to assist researchers and data analysts in extracting information from large numbers of political documents or any other text data at scale.


## Features

- Annotate text data using LLMs models
- Options for monitoring annotated texts in real-time while running
- Support for third-party LLM proxies
- Easy to resume from a break-point
- Save annotated files for future reference
  
## Installation

You can install the aitag package from GitHub using the `devtools` package:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install GPT-annotator from GitHub
devtools::install_github("yifeifrank/aitag")
```

## Usage

To use the GPT-annotator package, you need to set up your OpenAI API key and URL as system environment variables (no need to add v1/chat/completions in openai_url):

```r
Sys.setenv(openai_api = "your_api_key")
Sys.setenv(openai_url = "https://api.openai.com")
```

Then, load the package and use the `aitag` function to annotate your text data:

```r
conference_report <- conference_report |>  
  select(report_id,report) 

Sys.setenv("openai_key" = "")
Sys.setenv("openai_url" = "")
sys_prompt <- '<task>Based on the government conference report user provided, extract the participants of the conference, and their statement</task>
<json_example>
{"participent1":{"name":"name","statement":"statement"}}
</json_example>'

person <- 
  person |>    
  mutate(job_annotate=tag_gpt(text,
                              sys_prompt = sys_prompt,
                              model='gpt-4o-2024-05-13',
                              rate_limit = 0.1))
```

## Contributing

We welcome contributions to the GPT-annotator package. If you find a bug or have a feature request, please open an issue on the GitHub repository. If you would like to contribute code, please fork the repository and submit a pull request.

## Acknowledgments
This package is inspired by the code from Haohan Chen's lecture on annotating political documents with ChatGPT, available at [https://github.com/haohanchen/Lecture_ChatGPT](https://github.com/haohanchen/Lecture_ChatGPT).
I would like to thank Haohan Chen for providing the initial code and inspiration for this package through his lecture on annotating political documents with ChatGPT.

## License

This package is licensed under the [MIT License](LICENSE).
```
