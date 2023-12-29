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

describe('List Floors with Passage e2e tests', () => {
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

        cy.visit('/campus/floors/list-floors-with-passage')

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
        cy.title().should('equal', 'List Floors With Passage')
    })

    it('should have an empty selected building', () => {
        cy.get('#building').should('have.value', null)
    })

    it('should initially have an empty floors with passages list', () => {
        cy.get('.floors-with-passages-card').should('not.exist')
    })

    it('should select a building and display floors with passages', () => {
        cy.wait('@getBuildings')

        cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors/passages', {
            body: [
                {
                    floor: {
                        buildingCode: 'P',
                        floorNumber: 3,
                        description: 'example',
                    },
                    passages: [
                        {
                            buildingCode: 'O',
                            floorNumber: 1,
                            description: 'Floor1',
                        },
                        {
                            buildingCode: 'O',
                            floorNumber: 2,
                            description: 'Floor 2',
                        },
                    ],
                },
                {
                    floor: {
                        buildingCode: 'P',
                        floorNumber: 2,
                        description: 'asasd',
                    },
                    passages: [
                        {
                            buildingCode: 'O',
                            floorNumber: 1,
                            description: 'Floor1',
                        },
                        {
                            buildingCode: 'O',
                            floorNumber: 2,
                            description: 'Floor 2',
                        },
                    ],
                },
            ],
        }).as('getFloorsWithPassagesP')

        cy.get('#building').select('P')
        cy.wait('@getFloorsWithPassagesP')

        // Assertions for the displayed floors with passages data
        cy.get('.floors-with-passage-list').should('have.length', 1)

        // Assertions for the first floor with passages
        cy.get('.floors-card').eq(0).should('contain.text', 'Floor P3')
        cy.get('.floors-card').eq(0).should('contain.text', 'Description: example')

        // Assertions for the first passage of the first floor
        cy.get('.floors-card')
            .eq(0)
            .find('.floors-passage-card')
            .eq(0)
            .should('contain.text', 'Passage to O1')
        cy.get('.floors-card')
            .eq(0)
            .find('.floors-passage-card')
            .eq(0)
            .should('contain.text', 'Description: Floor1')

        // Assertions for the second passage of the first floor
        cy.get('.floors-card')
            .eq(0)
            .find('.floors-passage-card')
            .eq(1)
            .should('contain.text', 'Passage to O2')
        cy.get('.floors-card')
            .eq(0)
            .find('.floors-passage-card')
            .eq(1)
            .should('contain.text', 'Description: Floor 2')

        // Assertions for the second floor with passages
        cy.get('.floors-card').eq(1).should('contain.text', 'Floor P2')
        cy.get('.floors-card').eq(1).should('contain.text', 'Description: asasd')

        // Assertions for the first passage of the second floor
        cy.get('.floors-card')
            .eq(1)
            .find('.floors-passage-card')
            .eq(0)
            .should('contain.text', 'Passage to O1')
        cy.get('.floors-card')
            .eq(1)
            .find('.floors-passage-card')
            .eq(0)
            .should('contain.text', 'Description: Floor1')

        // Assertions for the second passage of the second floor
        cy.get('.floors-card')
            .eq(1)
            .find('.floors-passage-card')
            .eq(1)
            .should('contain.text', 'Passage to O2')
        cy.get('.floors-card')
            .eq(1)
            .find('.floors-passage-card')
            .eq(1)
            .should('contain.text', 'Description: Floor 2')
    })
})
