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

describe('Delete client Form e2e tests', () => {
    beforeEach(() => {
        window.localStorage.setItem('USER_ROLES', 'CLT')
        cy.visit('/delete-client')

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
        cy.title().should('equal', 'Delete Client')
    })

    it('should delete the client account', () => {
        cy.intercept('POST', 'http://localhost:4000/api/clients', {
            statusCode: 201,
            body: {
                name: 'quim',
                email: 'e2e-testing@isep.ipp.pt',
                phoneNumber: '123719765',
                vatNumber: '123189763',
                password: 'Passw0rd!T',
            },
        }).as('createClient')

        const email = 'e2e-testing@isep.ipp.pt'

        cy.intercept('DELETE', 'http://localhost:4000/clients/e2e-testing@isep.ipp.pt', {
            statusCode: 201,
            body: {
                email: email,
            },
        }).as('deleteClient')

        cy.get('button[type="submit"]').click()
    })
})
