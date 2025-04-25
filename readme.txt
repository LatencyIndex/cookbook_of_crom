Optimize cooking and nutrition.

Data
    Recipes
        Ingredient list
        Cook time
        Keep time (how many days you can eat it for)
    Ingredients
        Nutrients (micro, macro, calories)
        Usable fraction (how much you actually eat vs. what you throw away, e.g. potato skin, egg shell, etc.)
        Seasonal availability (1-month resolution)
    Product
        Which ingredients it contains
        Pricing (full price and location history)
    Inventory
        Products you already own
Capabilities
    Derive recipe properties from ingredients
        Nutrient information
        Price
    Create meal plan for N days
        Optimize for nutrients, price, and cook time (taking into consideration bulk cooking)
    Evaluate diet
        Given recipes/quantities eaten, output nutrient content and price.
        When reporting a nutrient, state the fraction of food that has a known quantity of that nutrient, from which it was derived, vs unknown.
            E.g. a meal of one egg (50g) and one apple (60g), where the egg has 10g/100g omega-3, and apple has unknown amount of omega 3, would report that the omega-3 content is 5g (50g known from 110g total, or 50/110)
    List recipes that can be cooked with inventory
