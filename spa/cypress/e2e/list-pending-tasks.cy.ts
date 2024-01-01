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

describe('List Pending Tasks e2e tests', () => {
    beforeEach(() => {
        cy.intercept('GET', 'http://localhost:4000/api/task?status=Pending', {
            body: [
                {
                    id: { value: '1' },
                    email: 'daniel@isep.ipp.pt',
                    jobType: 0,
                    status: 0,
                    location: {
                        startingPoint: { buildingCode: 'A', floorNumber: 1, x: 0, y: 0 },
                        endingPoint: { buildingCode: 'B', floorNumber: 2, x: 1, y: 1 },
                    },
                    surveillanceContact: {
                        name: 'Diogo Napoles',
                        phoneNumber: '123456789',
                    },
                },
                {
                    id: { value: '2' },
                    email: 'daniel@isep.ipp.pt',
                    jobType: 1,
                    status: 0,
                    location: {
                        startingPoint: { buildingCode: 'C', floorNumber: 3, x: 1, y: 2 },
                        endingPoint: { buildingCode: 'D', floorNumber: 4, x: 3, y: 4 },
                    },
                    description: 'Dispositivos da Cisco',
                    pickupContact: { name: 'Daniel', phoneNumber: '987654321' },
                    deliveryContact: { name: 'Joao Teixeira', phoneNumber: '987654321' },
                },
            ],
        }).as('getTasks')
        window.localStorage.setItem('USER_ROLES', 'ADM')

        cy.visit('/task' + '/list-pending')
        const log = Cypress.log({
            displayName: 'AUTH0 LOGIN',
            message: [`🔐 Authenticating | ${Cypress.env('auth_username')}`],
            // @ts-ignore
            autoEnd: false,
        })
        log.snapshot('before')

        loginViaAuth0Ui(Cypress.env('auth_username'), Cypress.env('auth_password'))

        log.snapshot('after')
        log.end()
    })

    it('has the correct title', () => {
        cy.title().should('equal', 'List pending tasks')
    })

    it('should have an empty task list', () => {
        cy.get('.building-card').should('not.exist')
    })

    it('should display pending tasks', () => {
        cy.wait('@getTasks')

        cy.get('.building-card').should('exist')
        cy.get('.building-card').should('contain.text', 'daniel@isep.ipp.pt')
        cy.get('.building-card').should('contain.text', 'Diogo Napoles')
        cy.get('.building-card').should('contain.text', 'Surveillance')

        cy.get('.building-card').should('contain.text', 'daniel@isep.ipp.pt')
        cy.get('.building-card').should('contain.text', 'Daniel')
        cy.get('.building-card').should('contain.text', 'Delivery')
    })
})
