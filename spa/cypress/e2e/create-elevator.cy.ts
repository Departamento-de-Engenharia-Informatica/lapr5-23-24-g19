describe('Elevator Form e2e tests', () => {
    beforeEach(() => {

        cy.intercept('GET', 'http://localhost:4000/api/buildings', {
            body: [
                {
                    code: 'P',
                    name: 'Civil2',
                    description: 'Departamento de Civil2',
                    maxFloorDimensions: {
                        length: 300,
                        width: 250,
                    },
                },
                {
                    code: 'O',
                    name: 'Informatic',
                    description: 'Informatic Department',
                    maxFloorDimensions: {
                        length: 20,
                        width: 30,
                    },
                },
            ],
        }).as('getBuildings')

        cy.visit('/campus/elevators/create');
    });

    it('has the correct title', () => {
        cy.title().should('equal', 'Create Elevator');
    });

    it('should have empty initial values', () => {
        cy.get('#selectedBuilding').should('have.value', null);
        cy.get('#selectedFloors').invoke('val').should('deep.equal', []);
        cy.get('#brand').should('have.value', '');
        cy.get('#model').should('have.value', '');
        cy.get('#serialNumber').should('have.value', '');
        cy.get('#description').should('have.value', '');
    });

  /*  it('should display error and disable submit button for invalid form', () => {
        // Submit the form without filling in any fields
        cy.get('button[type="submit"]').click({ force: true });

        // Wait for the error message to appear
        cy.get('.error-message', { timeout: 5000 }).should('exist'); // Increase timeout as needed

        // Assert disabled submit button
        cy.get('button[type="submit"]').should('be.disabled');
    });
*/


    it('should submit the form successfully and display created elevator', () => {

        cy.wait('@getBuildings')
        cy.get('#selectedBuilding').select('P');

        cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
            body: [
                {
                    buildingCode: 'P',
                    floorNumber: 1,
                    description: 'Physics Labs',
                },
                {
                    buildingCode: 'P',
                    floorNumber: 2,
                    description: 'Physics Labs',
                },
                {
                    buildingCode: 'P',
                    floorNumber: 3,
                    description: 'Physics Labs',
                },
            ],
        }).as('getFloors')

        cy.wait('@getFloors')

        cy.get('#selectedFloors').select(['1', '2']);
        cy.get('#brand').type('BrandName');
        cy.get('#model').type('ModelName');
        cy.get('#serialNumber').type('Serial123');
        cy.get('#description').type('This is a test elevator.');


        cy.get('button[type="submit"]').should('not.have.attr', 'disabled');

        cy.get('button[type="submit"]').click();
        //cy.get('button').contains('Submit').click()

        cy.get('form').submit();




        // Assert that the created elevator information is displayed
        cy.get('.elevator-list').should('exist');
        //cy.get('.elevator-card p').should('have.length.greaterThan', 0);
    });


});
