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

describe('Approve reject client e2e tests', () => {
    beforeEach(() => {
        cy.intercept('GET', 'http://localhost:4000/api/clients?state=Pending', {
            body: [
                { email: 'ricardo@isep.ipp.pt', name: 'Ricardo Reis' },
                { email: 'alberto@isep.ipp.pt', name: 'Alberto Caeiro' },
            ],
        }).as('getClients')
        window.localStorage.setItem('USER_ROLES', 'ADM')

        cy.visit('/admin' + '/approve-reject-client')
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
        cy.title().should('equal', 'Approve or reject client')
    })

    it('should have an empty client list', () => {
        cy.get('.building-card').should('not.exist')
    })

    it('should display pending approval clients', () => {
        cy.wait('@getClients')

        cy.get('.building-card').should('exist')
        cy.get('.building-card').should('contain.text', 'ricardo@isep.ipp.pt')
        cy.get('.building-card').should('contain.text', 'Ricardo Reis')

        cy.get('.building-card').should('contain.text', 'alberto@isep.ipp.pt')
        cy.get('.building-card').should('contain.text', 'Alberto Caeiro')
    })

    it('should be able to click approve client', () => {
        cy.intercept('GET', 'http://localhost:4000/api/clients?state=Pending', {
            body: [{ email: 'ricardo@isep.ipp.pt', name: 'Ricardo Reis' }],
        }).as('getClients')
        cy.wait('@getClients')

        cy.intercept('PATCH', 'http://localhost:4000/api/clients', {
            body: {
                body: { email: 'ricardo@isep.ipp.pt', name: 'Ricardo Reis' },
            },
        }).as('updateClient')

        cy.get('.building-card').should('exist')
        cy.get('.building-card').should('contain.text', 'ricardo@isep.ipp.pt')
        cy.get('.building-card').should('contain.text', 'Ricardo Reis')

        cy.intercept('GET', 'http://localhost:4000/api/clients?state=Pending', {
            body: [],
        }).as('getClients')

        cy.get('#approve').click()
        cy.wait('@updateClient')
        cy.wait('@getClients')

        cy.get('.building-card').should('not.exist')
    })

    it('should be able to click reject client', () => {
        cy.intercept('GET', 'http://localhost:4000/api/clients?state=Pending', {
            body: [{ email: 'ricardo@isep.ipp.pt', name: 'Ricardo Reis' }],
        }).as('getClients')
        cy.wait('@getClients')

        cy.intercept('PATCH', 'http://localhost:4000/api/clients', {
            body: {
                body: { email: 'ricardo@isep.ipp.pt', name: 'Ricardo Reis' },
            },
        }).as('updateClient')

        cy.get('.building-card').should('exist')
        cy.get('.building-card').should('contain.text', 'ricardo@isep.ipp.pt')
        cy.get('.building-card').should('contain.text', 'Ricardo Reis')

        cy.intercept('GET', 'http://localhost:4000/api/clients?state=Pending', {
            body: [],
        }).as('getClients')

        cy.get('#reject').click()
        cy.wait('@updateClient')
        cy.wait('@getClients')

        cy.get('.building-card').should('not.exist')
    })
})
