context('Edit Building Component', () => {
    beforeEach(() => {
        cy.intercept('GET', 'http://localhost:4000/api/buildings', {
            statusCode: 200,
            body: [
                { code: 'building1', name: 'Building 1', description: 'Description 1', maxFloorDimensions: { length: 10, width: 8 } },
                { code: 'building2', name: 'Building 2', description: 'Description 2', maxFloorDimensions: { length: 12, width: 10 } },
            ],
        }).as('getBuildings');
        cy.visit('/campus/buildings/edit')
    });

    it('should submit the form successfully on override', () => {
        cy.wait('@getBuildings')

        cy.intercept('PUT', 'http://localhost:4000/api/buildings/building1', {
            statusCode: 200,
            body: {
                code: 'building1',
                name: 'Updated Building 1',
                description: 'Updated Description 1',
                maxFloorDimensions: { length: 15, width: 12 },
            },
        }).as('putBuilding');

        cy.get('#buildingCode').select('building1');
        cy.get('#name').type('New Building Name');
        cy.get('#description').type('New Building Description');
        cy.get('#length').type("15");
        cy.get('#width').type("12");
        cy.get('#overrideConfirmation').check();

        cy.get('button[type="submit"]').click();

        cy.wait('@putBuilding');

        //clean values after submission
        cy.get('#name').should('have.value', '');
        cy.get('#description').should('have.value', '');
        cy.get('#length').should('have.value', '');
        cy.get('#width').should('have.value', '');
    });

    it('should submit the form successfully with a part of form camps filled', () => {
        cy.wait('@getBuildings')

        cy.intercept('PATCH', 'http://localhost:4000/api/buildings/building1', {
            statusCode: 200,
            body: {
                code: 'building1',
                name: 'Updated Building 1',
                description: 'Updated Description 1',
                maxFloorDimensions: { length: 15, width: 12 },
            },
        }).as('putBuilding');

        cy.get('button[type="submit"]').should('be.disabled');
        cy.get('#buildingCode').select('building1');
        cy.get('#name').type('New Building Name');
        cy.get('button[type="submit"]').should('be.enabled');
        cy.get('#description').type('New Building Description');
        cy.get('#length').type("15");
        cy.get('#width').type("12");

        cy.get('button[type="submit"]').click();

        cy.wait('@putBuilding');

        //clean values after submission
        cy.get('#name').should('have.value', '');
        cy.get('#description').should('have.value', '');
        cy.get('#length').should('have.value', '');
        cy.get('#width').should('have.value', '');
    });

    it('should show only be enabled when length and width are filled', () => {
        // Intercept the GET request to get buildings
        cy.intercept('GET', '/buildings', {
            statusCode: 200,
            body: [
                { code: 'building1', name: 'Building 1', description: 'Description 1', maxFloorDimensions: { length: 10, width: 8 } },
                { code: 'building2', name: 'Building 2', description: 'Description 2', maxFloorDimensions: { length: 12, width: 10 } },
            ],
        }).as('getBuildings');

        // Interact with the form and check button status
        cy.get('button[type="submit"]').should('be.disabled');
        cy.get('#buildingCode').select('building1');
        cy.get('button[type="submit"]').should('be.enabled');
        cy.get('#name').type('New Building Name');
        cy.get('button[type="submit"]').should('be.enabled');
        cy.get('#description').type('New Building Description');
        cy.get('button[type="submit"]').should('be.enabled');
        cy.get('#overrideConfirmation').check();
        cy.get('button[type="submit"]').should('be.disabled');
        cy.get('#length').type("10");
        cy.get('button[type="submit"]').should('be.disabled');
        cy.get('#width').type("0");
        cy.get('button[type="submit"]').should('be.enabled');

        cy.get('button[type="submit"]').click();
    });
});
