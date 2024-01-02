function loginViaAuth0Ui(username: string, password: string) {
    cy.origin(
        Cypress.env('auth_domain'),
        { args: { username, password } },
        ({ username, password }) => {
            cy.get('input#1-email').type(username)
            cy.get('input#1-password').type(password, { log: false })
            cy.get('button[type="submit"]')
                .should('be.visible')
                .should('contain.text', 'Log In')
                .should('not.be.disabled')
                .should('not.be.hidden')
                .click()
        },
    )
}

describe('CreateTaskSurveillanceComponent E2E Tests', () => {
    beforeEach(() => {
        // Mock API responses
        cy.intercept('GET', 'http://localhost:4000/api/buildings', {
            statusCode: 200,
            body: [
                {
                    code: "BuildingCode1",
                    name: "Main Building",
                    description: "Main administrative building",
                    maxFloorDimensions: { length: 100, width: 200 }
                },
                {
                    code: "BuildingCode2",
                    name: "Secondary Building",
                    description: "Secondary operations building",
                    maxFloorDimensions: { length: 150, width: 250 }
                }
            ]
        }).as('getBuildings');

        cy.intercept('GET', 'http://localhost:4000/api/buildings/BuildingCode1/floors', {
            statusCode: 200,
            body: [
                { buildingCode: "BuildingCode1", floorNumber: 1, description: "First floor of Main Building" },
                { buildingCode: "BuildingCode1", floorNumber: 2, description: "Second floor of Main Building" }
            ]
        }).as('getFloorsBuildingCode1');

        cy.intercept('GET', 'http://localhost:4000/api/buildings/BuildingCode2/floors', {
            statusCode: 200,
            body: [
                { buildingCode: "BuildingCode2", floorNumber: 1, description: "First floor of Secondary Building" },
                { buildingCode: "BuildingCode2", floorNumber: 2, description: "Second floor of Secondary Building" }
            ]
        }).as('getFloorsBuildingCode2');

        window.localStorage.setItem('USER_ROLES', 'CLT');
        cy.visit('/task/create/surveillance');

        // User authentication
        loginViaAuth0Ui(Cypress.env('auth_username'), Cypress.env('auth_password'));
    });

    it('loads the form if authenticated', () => {
        cy.get('form').should('exist');
    });

    it('fills and submits the surveillance task form', () => {
        // Fill in form fields
        cy.get('#email').type('user@example.com');
        cy.get('#selectedBuilding').select('BuildingCode1');
        cy.wait('@getFloorsBuildingCode1');
        cy.get('#selectedFloor').select('1');
        cy.get('#contactName').type('John Doe');
        cy.get('#contactPhone').type('123456789');

        cy.get('form').submit();

        cy.on('window:alert', (text) => {
            expect(text).to.contains('Task created successfully!');
        });
    });

    it('checks if select elements have the correct options', () => {
        // Check Building Select
        cy.get('#selectedBuilding').children('option').should('have.length.at.least', 2);
        cy.get('#selectedBuilding').find('option').then(options => {
            const actual = [...options].map(o => o.value);
            expect(actual).to.include.members(['BuildingCode1', 'BuildingCode2']);
        });

        // Check Floor Select for BuildingCode1
        cy.get('#selectedBuilding').select('BuildingCode1');
        cy.wait('@getFloorsBuildingCode1');
        cy.get('#selectedFloor').children('option').should('have.length.at.least', 2);
        cy.get('#selectedFloor').find('option').then(options => {
            const actual = [...options].map(o => o.textContent.trim());
            expect(actual).to.include.members(['1', '2']);
        });

        // Check Floor Select for BuildingCode2
        cy.get('#selectedBuilding').select('BuildingCode2');
        cy.wait('@getFloorsBuildingCode2');
        cy.get('#selectedFloor').children('option').should('have.length.at.least', 2);
        cy.get('#selectedFloor').find('option').then(options => {
            const actual = [...options].map(o => o.textContent.trim());
            expect(actual).to.include.members(['1', '2']);
        });
    });
});
