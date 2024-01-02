/*function loginViaAuth0Ui(username: string, password: string) {
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
}*/

describe('Client Form e2e tests', () => {
    beforeEach(() => {

    })

    it('has the correct title', () => {
        cy.title().should('equal', '')
    })

    it('should have empty initial values', () => {

        cy.visit('/create-client')

        cy.get('#username').should('have.value', '')
        cy.get('#email').should('have.value', '')
        cy.get('#vatNumber').should('have.value', '')
        cy.get('#phone').should('have.value', '')
        cy.get('#password').should('have.value', '')
        cy.get('#confirmPassword').should('have.value', '')
        cy.get('#privacyPolicy').should('not.be.checked');
    })


    it('should submit the form successfully and display created client', () => {

        cy.visit('/create-client')

        const name = 'quim'
        const email = 'joaquimfontes@isep.ipp.pt'
        const phoneNumber = '123789765'
        const vatNumber = '123789763'
        const password = 'Passw0rd!K'

        cy.get('#username').type(name)
        cy.get('#email').type(email)
        cy.get('#vatNumber').type(vatNumber)
        cy.get('#phone').type(phoneNumber)
        cy.get('#password').type(password)
        cy.get('#confirmPassword').type(password)
        cy.get('#privacyPolicy').check();


        cy.intercept('POST', 'http://localhost:4000/api/clients', {
            statusCode: 201,
            body: {
                name: name,
                email: email,
                phoneNumber: phoneNumber,
                vatNumber: vatNumber,
                password: password,
            },
        }).as('createClient')


        cy.get('button[type="submit"]').click()
        cy.wait('@createClient')

    })
})
