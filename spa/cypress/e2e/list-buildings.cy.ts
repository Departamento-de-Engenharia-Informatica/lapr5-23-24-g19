describe('GetBuildingsComponent', () => {
  beforeEach(() => {
    cy.intercept('GET', 'http://localhost:4000/api/buildings', {
      body: [
        {
          code: 'P',
          name: 'Physics',
          description: 'Physics Department',
          maxFloorDimensions: {
            length: 26,
            width: 12,
          },
        },
        {
          code: 'C',
          name: 'Chemistry',
          description: 'Chemistry Department',
          maxFloorDimensions: {
            length: 20,
            width: 15,
          },
        },
      ],
    }).as('getBuildings');

    cy.visit('campus/buildings/list');
  });

  it('should display the correct title', () => {
    cy.title().should('equal', 'List Buildings');
  });

  it('should initially have all buildings displayed', () => {
    cy.wait('@getBuildings');

    cy.get('.building-card').should('have.length', 2);
    cy.get('.building-card').should('contain.text', 'Physics');
    cy.get('.building-card').should('contain.text', 'Chemistry');
  });

  it('should show an error message when no buildings are found', () => {
    cy.intercept('GET', 'http://localhost:4000/api/buildings', {
      body: [],
    }).as('getBuildingsEmpty');

    cy.visit('campus/buildings/list');
    cy.wait('@getBuildingsEmpty');

    cy.get('.building-card').should('not.exist');
    //   cy.get('.error-popup').should('exist');
    //   cy.get('.error-popup').should('contain.text', 'Building Not Found');
  });


  it('should filter buildings based on user input', () => {
    // All buildings should be displayed
    cy.get('.building-card').should('have.length', 2);

    // Type 'Phy' in the search input to filter buildings
    cy.get('input[data-cy="search-input"]').type('Phy');

    cy.get('.building-card').should('have.length', 1);
    cy.get('.building-card').should('contain.text', 'Physics');
    cy.get('.dimension-box').should('exist');

    cy.get('input[data-cy="search-input"]').clear();

    cy.get('.building-card').should('have.length', 2);
    cy.get('.building-card').should('contain.text', 'Physics');
    cy.get('.building-card').should('contain.text', 'Chemistry');
  });
});
