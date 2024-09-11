
# Patrimoine Survey Data Hub: Research Collaboration and Harmonization

Welcome to the **Patrimoine Survey Data Hub**, a collaborative
repository designed to facilitate the harmonization and analysis of data
from the French *Patrimoine* Survey on household wealth. This repository
serves as a central platform for researchers working on various aspects
of the survey, providing tools for data harmonization, contextual
understanding, and support for international collaboration. It is part
of the WIDE project, a research group primarily based at *Institut
National d’Etudes Démographiques*, France, which focuses on studying the
consequences of demographic events on individuals’ wealth outcomes.
Information about the project is found
[here](https://wide.site.ined.fr/en/about/).

## The *Patrimoine* survey data

The Patrimoine survey aims to provide a comprehensive assessment of the
financial, professional, and property assets, as well as liabilities, of
individuals and households in France. It allows for both cross-household
comparisons and analyses of wealth distribution within households.
Beyond core wealth measures, the survey gathers detailed information on
familial and vocational histories, particularly focusing on
inheritances, donations, and income profiles. Conducted every six years
from 1986 to 2010, the survey shifted to a three-year interval
thereafter. Since 2014, a portion of households have also contributed to
longitudinal data.

The changes in survey design, along with shifts in French society, have
led to variations in question formulation and data post-processing over
time. These changes have introduced discrepancies in variable naming,
coding, and integration into statistical software. Additionally,
external variations arise from differing definitions of key concepts,
such as wealth and household composition, which can vary among
researchers depending on their methodologies and the accounting
standards they follow. This hub aims to address both internal
inconsistencies and these external conceptual variations, offering a
resource for researchers and users seeking a standardized approach to
analyzing the *Patrimoine* survey.

## Repository structure

This GitHub repository has three main objectives:

- **Common definitions and methods:** When detailed information is
  lacking, researchers may need to make assumptions regarding wealth,
  such as its distribution within the household how it is computed. This
  repository is designed to list, compare, and facilitate discussions
  about these different assumptions, providing users with informed
  options for their own analyses.

- **Harmonization tools:** The repository also includes a set of tools
  designed to help haronize survey data across different years. When
  pooled together, users may encounter discrepancies such as different
  variable names for the same information, variations in categories due
  to changes in survey design, or shifts in categories reflecting
  societal changes in France (e.g., degree reforms). This repository
  serves as a plce to document these changes and, ultimatley, offers
  strategies to address and resolve these discrepancies for consistent
  analysis.

- **Contextual documentation:** For international users or those
  unfamiliar with the French syste, this section offers detailed
  explanations of the French educational, financial, and institutional
  context, helping users accurately interpret the data.

For confidentiality purposes, the oriignal data is not included in this
GitHub repository. The data can be downloaded via
[Progedo](https://data.progedo.fr/studies/doi/10.13144/lil-1625) in a
limited version, where certain confidential variables are unavailable.
Full access to the data is provided through the
[CASD](https://www.casd.eu/source/enquete-histoire-de-vie-et-patrimoine/).
It is assumed that you have left the data untouched from the download,
i.e., all files should have a zip format. An R script is provided to
unzip those files and organize the data appropriately, allowing you to
run the remaining R scripts available on the platform.

## Contributing

To contribute, users with access to the *Patrimoine* survey data who
wish to participate in the shared codes and methodologies can contact
Marion Leturcq at the following address: <Marion.leturcq@ined.fr>.
Additionally, to request the inclusion of a specific variable in the
harmonization process, a dedicated flag has been set up in the “Issues”
section within the “scripts” folder.
