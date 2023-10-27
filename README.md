
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rgoogleclassroom

`rgoogleclassroom` is a Google API wrapper that allows you to use Google
Classroom and Google Forms from the coziness and comfort of R.

You can [read the rgoogleclassroom package documentation
here](https://datatrail-jhu.github.io/rgoogleclassroom/).

To use this package you need to have a Google Classroom account. Go here
to get that: <https://edu.google.com/workspace-for-education/classroom/>

## Installation

You can install this package from CRAN or the development version from
GitHub with:

``` r
# Install from CRAN
install.packages("rgoogleclassroom")

# or the development version from GitHub
if (!("remotes" %in% installed.packages())) {
  install.packages("remotes")
}
remotes::install_github("datatrail-jhu/rgoogleclassroom")
```

## Usage

To start, you need to `authorize()` the package to access your files.
Select all the scopes you feel comfortable sharing. Note that you need
to select certain scopes for certain functions to work.

``` r
library(rgoogleclassroom)

authorize()
```

### Basics

There are different objects on the Google API:

- courses
- forms
- courseWorks
- courseWork Materials
- topics

Most of these objects have functions that do the following:

- `get_<object>_list()`
- `create_<object>()`
- `get_<object>_properties()`

For example:

- `get_course_list(owner_id)` retrieves a list of courses for a
  particular owner id.
- `create_course()` creates a course
- `get_course_properties(course_id)` retrieves the properties of a
  course given its id.

These can be built together to be pretty nifty.

## Example workflow

Run the function to authorize the app to use your Google account.

``` r
authorize()
```

Retrieve whatever your owner id for Google Classroom is.

``` r
owner_id <- get_owner_id()
```

Now you can retrieve a list of courses that are associated with your
owner id.

``` r
course_df <- get_course_list(owner_id)
```

For the following examples, we will need to use

``` r
# Create a course
new_course <- create_course(owner_id$id, name = "New course")
```

### Managing materials

We can create new material for the students:

``` r
# Create a course we will use for this test
owner_id <- get_owner_id()
new_course <- create_course(owner_id$id, name = "New course")

# Create material at this course
new_material <- create_material(course_id = new_course$id,
                                title = "New material")

# Retrieve the material info
materials_info <- get_materials_properties(course_id = new_course$id,
                                           materials_id = new_material$id)
```

Retrieve the list of all the materials for the course:

``` r
materials_list <- get_materials_list(course_id = new_course$id)
```

### Managing courseworks

We can manage courseworks!

``` r
# Create a new coursework
new_coursework <- create_coursework(course_id = new_course$id,
                                    title = "New coursework",
                                    due_date = lubridate::today() + lubridate::hours(24))

# Get coursework properties
course_work_info <- get_coursework_properties(course_id = new_course$id, coursework_id = new_coursework$id)

# Retrieve all the courseworks for this course
coursework_list <- get_coursework_list(course_id = new_course$id)
```

### Make a quiz

We can build a quiz like this:

``` r
course_id <- get_course_list()$courses$id[1]

quiz_form_id <- create_quiz(
  course_id = course_id,
  quiz_title = "new quiz2",
  quiz_description = "This is a great quiz",
  due_date = "2025-12-1")
```

We can create a new multiple choice question in the quiz we just made by
using these steps:

``` r
create_multiple_choice_question(
  form_id = quiz_form_id$form_info$formId,
  question = "What answer do you want?",
  choice_vector = c("A", "B", "C", "D"),
  correct_answer = 3,
  shuffle_opt = TRUE
)
```

### Delete or archive courses

``` r
# Clean up this test course
archive_course(new_course$id)
delete_course(new_course$id)
```
