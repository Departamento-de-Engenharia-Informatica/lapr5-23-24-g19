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
describe('List Elevators e2e tests', () => {
    beforeEach(() => {
        cy.intercept('GET', 'http://localhost:4000/api/buildings', {
            body: [
                {
                    code: 'P',
                    name: 'Civil2',
                    description: 'Departamento de Civil2',
                    maxFloorDimensions: {
                        length: 300,
                        width: 250,
                    },
                },
                {
                    code: 'O',
                    name: 'Informatic',
                    description: 'Informatic Department',
                    maxFloorDimensions: {
                        length: 20,
                        width: 30,
                    },
                },
            ],
        }).as('getBuildings')
        window.localStorage.setItem('USER_ROLES', 'ADM')

        cy.visit('/campus/elevators/list')

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
        cy.title().should('equal', 'List Elevators')
    })

    it('should have an empty selected building', () => {
        cy.get('#building').should('have.value', null)
    })

    it('should initially have an empty elevator list', () => {
        cy.get('.elevator-card').should('not.exist')
    })

    it('should select a building and display elevators', () => {
        cy.wait('@getBuildings')
        cy.intercept('GET', 'http://localhost:4000/api/buildings/P/elevators', {
            body: [
                {
                    identifier: 'E1',
                    floors: 5,
                    brand: 'Brand1',
                    model: 'Model1',
                    serialNumber: 'SN1',
                    description: 'Physics Elevator 1',
                },
            ],
        }).as('getElevatorsP')

        cy.get('#building').select('P')

        cy.wait('@getElevatorsP')

        cy.get('.elevator-card').should('exist')
        cy.get('.elevator-card').should('contain.text', 'Identifier: E1')
        cy.get('.elevator-card').should('contain.text', 'Floors: 5')
        cy.get('.elevator-card').should('contain.text', 'Brand: Brand1')
        cy.get('.elevator-card').should('contain.text', 'Model: Model1')
        cy.get('.elevator-card').should('contain.text', 'Serial Number: SN1')
        cy.get('.elevator-card').should('contain.text', 'Description: Physics Elevator 1')

        cy.intercept('GET', 'http://localhost:4000/api/buildings/O/elevators', {
            body: [
                {
                    identifier: 'E2',
                    floors: 3,
                    brand: 'Brand2',
                    model: 'Model2',
                    serialNumber: 'SN2',
                    description: 'Chemistry Elevator 1',
                },
                {
                    identifier: 'E3',
                    floors: 7,
                    brand: 'Brand3',
                    model: 'Model3',
                    serialNumber: 'SN3',
                    description: '',
                },
            ],
        }).as('getElevatorsO')

        cy.get('#building').select('O')
        cy.wait('@getElevatorsO')

        cy.get('.elevator-card').should('exist')
        cy.get('.elevator-card').should('contain.text', 'Identifier: E2')
        cy.get('.elevator-card').should('contain.text', 'Floors: 3')
        cy.get('.elevator-card').should('contain.text', 'Brand: Brand2')
        cy.get('.elevator-card').should('contain.text', 'Model: Model2')
        cy.get('.elevator-card').should('contain.text', 'Serial Number: SN2')
        cy.get('.elevator-card').should(
            'contain.text',
            'Description: Chemistry Elevator 1',
        )
        cy.get('.elevator-card').should('contain.text', 'Identifier: E3')
        cy.get('.elevator-card').should('contain.text', 'Floors: 7')
        cy.get('.elevator-card').should('contain.text', 'Brand: Brand3')
        cy.get('.elevator-card').should('contain.text', 'Model: Model3')
        cy.get('.elevator-card').should('contain.text', 'Serial Number: SN3')
        cy.get('.elevator-card').should('contain.text', 'No description')
    })
})
