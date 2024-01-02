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

describe('aa', () => {
    beforeEach(() => {

        window.localStorage.setItem('USER_ROLES', 'TKM');

        cy.visit('/task/filter');

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
        cy.title().should('equal', 'Filter tasks');
    });

    it('loads the filter form', () => {
        cy.get('.container').should('exist');
        cy.get('form').should('exist');
    });

    it('submits the filter form for client and displays results', () => {
        cy.intercept('GET', 'http://localhost:4000/api/task/filter?criteria=Client&rule=john.doe@example.com', {
            statusCode: 200,
            body: [
                {
                    email: "john.doe@example.com",
                    location: {
                        startingPoint: {
                            buildingCode: "A1",
                            floorNumber: 1,
                            x: 100,
                            y: 200
                        },
                        endingPoint: {
                            buildingCode: "B2",
                            floorNumber: 3,
                            x: 150,
                            y: 250
                        }
                    },
                    status: 0,
                    jobType: 1,
                    id: {
                        value: "task-123"
                    },
                    surveillanceContact: {
                        name: "Jane Smith",
                        phoneNumber: "123-456-7890"
                    },
                    pickupContact: {
                        name: "Alan Turing",
                        phoneNumber: "123-456-7891"
                    },
                    deliveryContact: {
                        name: "Ada Lovelace",
                        phoneNumber: "123-456-7892"
                    },
                    confirmationCode: {
                        code: 9876
                    },
                    description: "Deliver important documents"
                },
            ],
        }).as('getFilteredTasks');  

        cy.get('#criteria').select('Client');
        cy.get('#rule').type('john.doe@example.com');
        cy.get('.button-submit').click();

        cy.wait('@getFilteredTasks').its('response.statusCode').should('eq', 200);

        cy.get('.building-list').should('have.length', 1);

        cy.get('.building-card').eq(0).should('contain.text', "john.doe@example.com");
    });

    it('submits the filter form for type and displays results', () => {
        cy.intercept('GET', 'http://localhost:4000/api/task/filter?criteria=Type&rule=0', {
            statusCode: 200,
            body: [
                {
                    email: "john.doe@example.com",
                    location: {
                        startingPoint: {
                            buildingCode: "A1",
                            floorNumber: 1,
                            x: 100,
                            y: 200
                        },
                        endingPoint: {
                            buildingCode: "B2",
                            floorNumber: 3,
                            x: 150,
                            y: 250
                        }
                    },
                    status: 0,
                    jobType: 0,
                    id: {
                        value: "task-123"
                    },
                    surveillanceContact: {
                        name: "Jane Smith",
                        phoneNumber: "123-456-7890"
                    },
                    pickupContact: {
                        name: "Alan Turing",
                        phoneNumber: "123-456-7891"
                    },
                    deliveryContact: {
                        name: "Ada Lovelace",
                        phoneNumber: "123-456-7892"
                    },
                    confirmationCode: {
                        code: 9876
                    },
                    description: "Deliver important documents"
                },
            ],
        }).as('getFilteredTasks');  
        cy.get('#criteria').select('Type');
        cy.get('#rule').select('Surveillance');

        cy.get('.button-submit').click();

        cy.wait('@getFilteredTasks').its('response.statusCode').should('eq', 200);

        cy.get('.building-list').should('have.length', 1);

        cy.get('.building-card').eq(0).should('contain.text', "Surveillance");
    });
    it('checks the select values', () => {
        cy.get('#criteria').children().should('have.length.at.least', 2);
        cy.get('#criteria').find('option').then(options => {
            const actual = [...options].map(o => o.value);
            expect(actual).to.include.members(['Client', 'State', 'Type']);
        });
    });
    it('checks the type values', () => {
        cy.get('#criteria').select('Type');
        cy.get('#rule').children().should('have.length', 2);
        cy.get('#rule').find('option').then(options => {
            const actual = [...options].map(o => o.value);
            expect(actual).to.include.members(['Surveillance', 'Delivery']);
        });
    });
    it('checks the state values', () => {
        cy.get('#criteria').select('State');
        cy.get('#rule').children().should('have.length', 4);
        cy.get('#rule').find('option').then(options => {
            const actual = [...options].map(o => o.value);
            expect(actual).to.include.members(['Pending', 'Approved', 'Rejected', 'Planned']);
        });
    });
    
    it('displays no tasks template when there are no tasks', () => {
        cy.intercept('GET', 'http://localhost:4000/api/task/filter*', {
            statusCode: 200,
            body: [],
        }).as('getNoTasks');
    
        cy.get('#criteria').select('Client');
        cy.get('#rule').type('nonexistent@example.com');
        cy.get('.button-submit').click();
    
        cy.wait('@getNoTasks');
        cy.contains('Tasks not found for the provided filter.').should('be.visible');

    });
});
