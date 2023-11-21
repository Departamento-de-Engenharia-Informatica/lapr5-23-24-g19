describe('Passages e2e tests', () => {
    beforeEach(() => {
        cy.intercept('GET', 'http://localhost:4200/api/buildings', {
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

        cy.visit('/campus/passages/list-passages-between-buildings');
    });

    it('has the correct title', () => {
        cy.title().should('equal', 'List passages between buildings');
    });

    it('should have empty selected buildings', () => {
        cy.get('#building1').eq(0).should('have.value', null);
        cy.get('#building2').eq(0).should('have.value', null);
    });

    it('should initially have no passages displayed', () => {
        cy.get('.list-passages-between-buildings-card').should('not.exist');
    });

    it('should select buildings and display passages', () => {
        const listPassages = cy.get('.list-passages-between-buildings-list');

        cy.intercept('GET', 'http://localhost:4200/api/passages', {
            body: [
                {
                    floor1: {
                        buildingCode: 'O',
                        floorNumber: 2,
                    },
                    floor2: {
                        buildingCode: 'P',
                        floorNumber: 3,
                    },
                },
            ],
        }).as('getPassages');


        cy.get('#building1').eq(0).select('P');
        cy.get('#building2').eq(0).select('O');



        listPassages.get('.list-passages-between-buildings-card').should('exist');
        listPassages.get('h3').should('contain.text', 'Floor1');
        listPassages.get('h4').should('contain.text', 'Floor2');
        listPassages.get('p').should('contain.text', 'buildingCode: O');
        listPassages.get('p').should('contain.text', 'floorNumber: 2');
        listPassages.get('p').should('contain.text', 'buildingCode: P');
        listPassages.get('p').should('contain.text', 'floorNumber: 3');
    });

});
