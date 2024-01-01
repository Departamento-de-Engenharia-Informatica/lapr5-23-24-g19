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

describe('Edit client e2e tests', () => {
    beforeEach(() => {
        cy.intercept('GET', 'http://localhost:4000/api/clients/e2e-testing@isep.ipp.pt', {
            body: {
                email: 'e2e-testing@isep.ipp.pt',
                name: 'Bruno Silva',
                phoneNumber: '123456789',
                vatNumber: 123456789,
            },
        }).as('getClient')
        window.localStorage.setItem('USER_ROLES', 'CLT')

        cy.visit('/edit-client')
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
        cy.title().should('equal', 'Edit Client')
    })

    it('should display the form elements', () => {
        cy.get('#clientName').should('exist')
        cy.get('#clientPhoneNumber').should('exist')
        cy.get('#clientVatNumber').should('exist')
    })

    it('should have placeholder text', () => {
        cy.get('#clientName').should('have.attr', 'placeholder', 'Bruno Silva')
        cy.get('#clientPhoneNumber').should('have.attr', 'placeholder', '123456789')
        cy.get('#clientVatNumber').should('have.attr', 'placeholder', '123456789')
    })

    it('submit button should be disabled if no changes are made', () => {
        cy.get('button[type="submit"]').should('be.disabled')
    })

    it("submit button should be enabled if changes are made to the client's name", () => {
        cy.get('#clientName').type('Bruno Silva')
        cy.get('button[type="submit"]').should('be.enabled')
    })

    it("submit button should be enabled if changes are made to the client's phone number", () => {
        cy.get('#clientPhoneNumber').type('123456789')
        cy.get('button[type="submit"]').should('be.enabled')
    })

    it("submit button should be enabled if changes are made to the client's VAT number", () => {
        cy.get('#clientVatNumber').type('123456789')
        cy.get('button[type="submit"]').should('be.enabled')
    })

    it('submit button should be disabled if phone number is invalid', () => {
        cy.get('#clientPhoneNumber').type('1234567894132412')
        cy.get('button[type="submit"]').should('be.disabled')
    })

    it('submit button should be disabled if VAT number is invalid', () => {
        cy.get('#clientVatNumber').type('1234567894132412')
        cy.get('button[type="submit"]').should('be.disabled')
    })

    it('should be able to submit changes and placeholders should update', () => {
        cy.intercept(
            'PATCH',
            'http://localhost:4000/api/clients/e2e-testing@isep.ipp.pt',
            {
                body: {
                    email: 'e2e-testing@isep.ipp.pt',
                    name: 'Bruno Costa',
                    phoneNumber: '123432123',
                    vatNumber: 876123456,
                },
            },
        ).as('patchClient')

        cy.get('#clientName').type('Bruno Costa')
        cy.get('#clientPhoneNumber').type('123432123')
        cy.get('#clientVatNumber').type('876123456')

        cy.get('button[type="submit"]').click()
        cy.wait('@patchClient')

        cy.get('#clientName').should('have.attr', 'placeholder', 'Bruno Costa')
        cy.get('#clientPhoneNumber').should('have.attr', 'placeholder', '123432123')
        cy.get('#clientVatNumber').should('have.attr', 'placeholder', '876123456')
    })
})
