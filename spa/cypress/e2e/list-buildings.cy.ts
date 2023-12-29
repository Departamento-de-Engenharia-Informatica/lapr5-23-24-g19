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

describe('GetBuildingsComponent', () => {
    beforeEach(() => {
        cy.intercept('GET', 'http://localhost:4000/api/buildings', {
            body: [
                {
                    code: 'P',
                    name: 'Physics',
                    description: 'Physics Department',
                    maxFloorDimensions: {
                        length: 26,
                        width: 12,
                    },
                },
                {
                    code: 'C',
                    name: 'Chemistry',
                    description: 'Chemistry Department',
                    maxFloorDimensions: {
                        length: 20,
                        width: 15,
                    },
                },
            ],
        }).as('getBuildings')
        window.localStorage.setItem('USER_ROLES', 'ADM')

        cy.visit('campus/buildings/list')
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

    it('should display the correct title', () => {
        cy.title().should('equal', 'List Buildings')
    })

    it('should initially have all buildings displayed', () => {
        cy.wait('@getBuildings')

        cy.get('.building-card').should('have.length', 2)
        cy.get('.building-card').should('contain.text', 'Physics')
        cy.get('.building-card').should('contain.text', 'Chemistry')
    })

    it('should filter buildings based on user input', () => {
        // All buildings should be displayed
        cy.get('.building-card').should('have.length', 2)

        // Type 'Phy' in the search input to filter buildings
        cy.get('input[data-cy="search-input"]').type('Phy')

        cy.get('.building-card').should('have.length', 1)
        cy.get('.building-card').should('contain.text', 'Physics')
        cy.get('.dimension-box').should('exist')

        cy.get('input[data-cy="search-input"]').clear()

        cy.get('.building-card').should('have.length', 2)
        cy.get('.building-card').should('contain.text', 'Physics')
        cy.get('.building-card').should('contain.text', 'Chemistry')
    })
})
