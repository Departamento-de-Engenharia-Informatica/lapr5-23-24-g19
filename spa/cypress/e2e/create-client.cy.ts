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

describe('Client Form e2e tests', () => {
    beforeEach(() => {

        // TODO:NAO ESTA A FUNCIONAR PARA JA POR CAUSA DO cy.visit... assim que tivermos a pagina do create client direita atribuida sera so mudar o link dentro do cy visit

        /*window.localStorage.setItem('USER_ROLES', 'ADM')
        cy.visit('/create-client')

        const log = Cypress.log({
            displayName: 'AUTH0 LOGIN',
            message: [`🔐 Authenticating | ${Cypress.env('auth_username')}`],
            // @ts-ignore
            autoEnd: false,
        })
        log.snapshot('before')

        loginViaAuth0Ui(Cypress.env('auth_username'), Cypress.env('auth_password'))

        log.snapshot('after')
        log.end()*/
    })

    it('has the correct title', () => {
        // TODO:NAO ESTA A FUNCIONAR PARA JA POR CAUSA DO titulo, ainda nao atribuido a pagina
        //cy.title().should('equal', 'Create Client')
    })

    it('should have empty initial values', () => {

        //TODO: assim que o resto do codigo em cima estiver operacional remover este cy.visit
        cy.visit('/create-client')





        cy.get('#name').should('have.value', '')
        cy.get('#email').should('have.value', '')
        cy.get('#phoneNumber').should('have.value', '')
        cy.get('#vatNumber').should('have.value', '')
        cy.get('#password').should('have.value', '')
        cy.get('#privacyPolicy').should('not.be.checked');
    })


    it('should submit the form successfully and display created client', () => {

        //TODO: assim que o resto do codigo em cima estiver operacional remover este cy.visit
        cy.visit('/create-client')




        const name = 'quim'
        const email = 'joaquimfontes@isep.ipp.pt'
        const phoneNumber = '123789765'
        const vatNumber = '123789763'
        const password = 'Passw0rd!K'

        cy.get('#name').type(name)
        cy.get('#email').type(email)
        cy.get('#phoneNumber').type(phoneNumber)
        cy.get('#vatNumber').type(vatNumber)
        cy.get('#password').type(password)
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
