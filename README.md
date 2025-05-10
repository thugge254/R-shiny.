# Insurance Pricing Tool - R Shiny App

This interactive R Shiny application is designed to compute and visualize expected insurance reserves

and premiums for life insurance policies. It offers users a dynamic interface to explore how various insurance parameters

affect both reserves and premiums, with specific modules for single and couple policies.

## Reserve Plot

- An interactive line plot visualizing the expected reserve over time.

- Responds in real-time to user inputs and clearly shows how reserve values evolve over a specified time frame.

##  Premium Calculation

- Displays the expected premium in an infoBox, computed based on input parameters.

- Expressed in dollars (`$`), dynamically updated with every input change.

##  Control Panel

Fully interactive sidebar menus for:

- Single Policy

- Couple Policy

### Parameters include:

Product type (e.g., assurance, annuity)

- Age

- Interest rates

- Assured sum

- Annuity product type

- Premium payments

- Payment frequency

- Expenses, and more.

- Conditional panels dynamically show or hide variables depending on user selection, ensuring a clean and relevant interface.

### 📁 Folder Structure
```
insurance-pricing-app/
│
├── ui.R
├── server.R
├── www/ (assets for CSS or images)
├── data/
└── README.md
````
### License

This project is open-source and is licensed under the MIT License.
