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

describe('Backoffice user Form e2e tests', () => {
    beforeEach(() => {

        window.localStorage.setItem('USER_ROLES', 'ADM')

        cy.visit('/admin' + '/create-backoffice')
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
        cy.title().should('equal', 'Create backoffice user')
    })

    it('should have empty initial values', () => {

        cy.get('#username').should('have.value', '')
        cy.get('#email').should('have.value', '')
        cy.get('#phone').should('have.value', '')
        cy.get('#password').should('have.value', '')
        cy.get('#confirmPassword').should('have.value', '')

    })


    it('should submit the form successfully and display created Backoffice user', () => {

        cy.intercept('GET', 'http://localhost:4000/api/roles', {
            body: [
                { name: 'Campus Manager' },
                { name: 'Task Manager' },
                { name: 'Fleet Manager' }
            ],
        }).as('getRoles')

        const name = 'quim'
        const role = 'Fleet Manager'
        const email = 'joaquimfontesxto@isep.ipp.pt'
        const phoneNumber = '123781265'
        const password = 'Passw0rd!Fx'

        cy.wait('@getRoles');

        cy.get('#role').select(role)
        cy.get('#username').type(name)
        cy.get('#email').type(email)
        cy.get('#phone').type(phoneNumber)
        cy.get('#password').type(password)
        cy.get('#confirmPassword').type(password)

        cy.intercept('POST', 'http://localhost:4000/api/users-backoffice', {
            statusCode: 201,
            body: {
                name: name,
                role: role,
                email: email,
                phoneNumber: phoneNumber,
                password: password,
            },
        }).as('createBackofficeUser')


        cy.get('button[type="submit"]').click()
        cy.wait('@createBackofficeUser')

        cy.on('window:alert', (text) => {
            expect(text).to.contain('Created user with success')
        })
    })

    it('shouldn\'t allow creation if passwords do not match', () => {

        cy.intercept('GET', 'http://localhost:4000/api/roles', {
            body: [
                { name: 'Campus Manager' },
                { name: 'Task Manager' },
                { name: 'Fleet Manager' }
            ],
        }).as('getRoles')

        const name = 'quim'
        const role = 'Fleet Manager'
        const email = 'joaquimfontesxto@isep.ipp.pt'
        const phoneNumber = '123781265'
        const password = 'Passw0rd!Fx'

        cy.wait('@getRoles');

        cy.get('#role').select(role)
        cy.get('#username').type(name)
        cy.get('#email').type(email)
        cy.get('#phone').type(phoneNumber)
        cy.get('#password').type(password)
        cy.get('#confirmPassword').type(password+'11111')

        cy.get('button[type="submit"]').click()

        cy.on('window:alert', (text) => {
            expect(text).to.contain('Passwords do not match')
        })
    })
})
