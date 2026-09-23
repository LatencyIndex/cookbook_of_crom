# Cookbook of Crom

Optimize your diet to get required nutrients.

Shall use usda_parser to generate Haskell source modules with nutrient content for various food. This will provide compile-time assurance that recipes have all required data with correct dimensions, and avoid the need for Cookbook of Crom itself to include a parser.

## Capabilities

- [x] Derive nutrient content from recipe ingredients
- [ ] Show nutrient deficits and excesses for a given diet
- [ ] Create meal plan for N days
- [ ] Filter recipes by available ingredients
- [ ] Filter recipes by diet - omnivore, vegetarian, vegan, allergies, gluten, etc.

## Data sources

### Nutrient data

USDA: https://fdc.nal.usda.gov/download-datasets.html
Gnutrition (also sources from USDA, used for double-checking): https://www.gnu.org/software/gnutrition/

Secondary unused sources:
- https://tools.myfooddata.com/
- https://foodb.ca/

### Seasonal availability

https://in-season.gocemitevski.com/ (unknown reliability)
