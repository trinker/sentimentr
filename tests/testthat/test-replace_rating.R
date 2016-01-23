context("Checking replace_rating")

test_that("replace_rating replaces ratings",{

    x <- c("This place receives 5 stars for their APPETIZERS!!!",
         "Four stars for the food & the guy in the blue shirt for his great vibe!",
         "10 out of 10 for both the movie and trilogy.",
         "* Both the Hot & Sour & the Egg Flower Soups were absolutely 5 Stars!",
         "For service, I give them no stars.", "This place deserves no stars.",
         "10 out of 10 stars.",
         "My rating: just 3 out of 10.",
         "If there were zero stars I would give it zero stars.",
         "Rating: 1 out of 10.",
         "I gave it 5 stars because of the sound quality.",
         "If it were possible to give them 0/10, they'd have it."
    )
    
    x2 <- c("This place receives best for their APPETIZERS!!!", " better for the food & the guy in the blue shirt for his great vibe!", 
        " best for both the movie and trilogy.", "* Both the Hot & Sour & the Egg Flower Soups were absolutely best !", 
        "For service, I give them terrible .", "This place deserves terrible .", 
        " best stars.", "My rating: just below average .", "If there were terrible I would give it terrible .", 
        "Rating: extremely below average .", "I gave it best because of the sound quality.", 
        "If it were possible to give them terrible , they'd have it."
        )
    
    expect_equal(replace_rating(x), x2)


})

