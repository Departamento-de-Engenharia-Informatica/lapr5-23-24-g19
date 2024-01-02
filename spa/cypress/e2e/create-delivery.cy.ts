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
describe('CreateTaskDeliveryComponent E2E Tests', () => {
    beforeEach(() => {
        cy.intercept('GET', 'http://localhost:4000/api/buildings', {
            statusCode: 200,
            body:
                [
                    {
                        "code": "BuildingCode1",
                        "name": "Main Building",
                        "description": "Main administrative building",
                        "maxFloorDimensions": { "length": 100, "width": 200 }
                    },
                    {
                        "code": "BuildingCode2",
                        "name": "Secondary Building",
                        "description": "Secondary operations building",
                        "maxFloorDimensions": { "length": 150, "width": 250 }
                    }
                ]
        }).as('getBuildings');
        cy.intercept('GET', 'http://localhost:4000/api/buildings/BuildingCode1/floors', {
            statusCode: 200,
            body:
                [
                    {
                        "buildingCode": "BuildingCode1",
                        "floorNumber": 1,
                        "description": "First floor of Main Building"
                    },
                    {
                        "buildingCode": "BuildingCode1",
                        "floorNumber": 2,
                        "description": "Second floor of Secondary Building"
                    }

                ]
        }).as('getFloors1');
        cy.intercept('GET', 'http://localhost:4000/api/buildings/BuildingCode2/floors', {
            statusCode: 200,
            body:
                [
                    {
                        "buildingCode": "BuildingCode2",
                        "floorNumber": 1,
                        "description": "First floor of Main Building"
                    },
                    {
                        "buildingCode": "BuildingCode2",
                        "floorNumber": 2,
                        "description": "Second floor of Secondary Building"
                    }

                ]
        }).as('getFloors2')
        cy.intercept('GET', 'http://localhost:4000/api/buildings/BuildingCode1/floors/1/rooms', {
            statusCode: 200,
            body: [
                {
                    "name": "Room1",
                    "buildingCode": "BuildingCode1",
                    "floorNumber": 1,
                    "description": "Conference Room",
                    "category": "Meeting",
                    "dimensions": { "length": 20, "width": 30 },
                    "positions": { "x": 10, "y": 15 }
                }
            ]
        }).as('getRooms1');
        cy.intercept('GET', 'http://localhost:4000/api/buildings/BuildingCode2/floors/2/rooms', {
            statusCode: 200,
            body: [
                {
                    "name": "Room2",
                    "buildingCode": "BuildingCode2",
                    "floorNumber": 2,
                    "description": "Office Room",
                    "category": "Office",
                    "dimensions": { "length": 25, "width": 35 },
                    "positions": { "x": 12, "y": 18 }
                }
            ]

        }).as('getRooms2');

        window.localStorage.setItem('USER_ROLES', 'CLT');

        cy.visit('/task/create/delivery');

        const log = Cypress.log({
            displayName: 'AUTH0 LOGIN',
            message: [`🔐 Authenticating | ${Cypress.env('auth_username')}`],
            autoEnd: false,
        });
        log.snapshot('before');
        loginViaAuth0Ui(Cypress.env('auth_username'), Cypress.env('auth_password'));
        log.snapshot('after');
        log.end();
    });
    it('has the correct title', () => {
        cy.title().should('equal', 'Create delivery task');
    });

    it('should load the form if authenticated', () => {
        cy.get('form').should('exist');
    });

    it('fills and submits the delivery task form', () => {
        // Fill out the form
        cy.get('#description').type('Deliver documents');
        cy.get('#confirmationCode').type('123456');

        // Select building, floor, and room for both pickup and delivery
        cy.get('#selectedBuilding1').select('BuildingCode1');
        cy.wait('@getFloors1');
        cy.get('#selectedFloor1').select('1');
        cy.wait('@getRooms1');
        cy.get('#selectedRoom1').select('Room1');

        cy.get('#pickupContactName').type('John Doe');
        cy.get('#pickupContactPhone').type('123456789');

        // Repeat for delivery options
        cy.get('#selectedBuilding2').select('BuildingCode2');
        cy.wait('@getFloors2');
        cy.get('#selectedFloor2').select('2');
        cy.wait('@getRooms2');
        cy.get('#selectedRoom2').select('Room2');
        cy.get('#deliveryContactName').type('Jane Doe');
        cy.get('#deliveryContactPhone').type('987654321');

        cy.get('form').submit();

        cy.on('window:alert', (text) => {
            expect(text).to.contains('Task created successfully!');
        });
    });
    it('checks if select elements have the correct options', () => {
        cy.get('#selectedBuilding1').children('option').should('have.length.at.least', 2);
        cy.get('#selectedBuilding1').find('option').then(options => {
            const actual = [...options].map(o => o.value);
            expect(actual).to.include.members(['BuildingCode1', 'BuildingCode2']);
        });

        cy.get('#selectedBuilding1').select('BuildingCode1');
        cy.wait('@getFloors1');
        cy.get('#selectedFloor1').children('option').should('have.length.at.least', 2);
        cy.get('#selectedFloor1').find('option').then(options => {
            const actual = [...options].map(o => o.value.trim());
            expect(actual).to.include.members(['1', '2']);
        });

        cy.get('#selectedFloor1').select('1');
        cy.wait('@getRooms1');
        cy.get('#selectedRoom1').children('option').should('have.length.at.least', 1);
        cy.get('#selectedRoom1').find('option').then(options => {
            const actual = [...options].map(o => o.value.trim());
            expect(actual).to.include.members(['Room1']);
        });
    });
});
