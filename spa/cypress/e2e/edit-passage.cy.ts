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

describe('Edit passage e2e tests', () => {
    beforeEach(() => {
        cy.intercept('GET', 'http://localhost:4000/api/buildings', {
            body: [
                { code: 'P', name: 'Physics' },
                { code: 'C', name: 'Chemistry' },
            ],
        }).as('getBuildings')
        window.localStorage.setItem('USER_ROLES', 'ADM')

        cy.visit('campus/passages/edit')

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

    it('should display the form with options to edit a passage', () => {
        cy.wait('@getBuildings')

        cy.get('#oldBuildingCode1').should('exist')
        cy.get('#oldBuildingCode2').should('exist')
        cy.get('#selectPassage').should('exist')
        cy.get('#newBuildingCode1').should('exist')
        cy.get('#newBuildingCode2').should('exist')
        cy.get('#newFloorNumber1').should('exist')
        cy.get('#newFloorNumber2').should('exist')
        cy.get('button').should('exist')
    })

    it('should submit the form with valid input', () => {
        cy.wait('@getBuildings')

        cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
            body: [{ floorNumber: 1 }, { floorNumber: 2 }],
        }).as('getFloorsPhysics')

        cy.intercept('GET', 'http://localhost:4000/api/buildings/C/floors', {
            body: [{ floorNumber: 1 }, { floorNumber: 2 }],
        }).as('getFloorsChemistry')

        cy.intercept(
            'GET',
            'http://localhost:4000/api/passages?building1=P&building2=C',
            {
                body: [
                    {
                        floor1: {
                            buildingCode: 'P',
                            floorNumber: 1,
                        },
                        floor2: {
                            buildingCode: 'C',
                            floorNumber: 2,
                        },
                    },
                ],
            },
        ).as('getPassages')

        cy.get('#oldBuildingCode1').select('P')

        cy.get('#oldBuildingCode2').select('C')
        cy.wait('@getPassages')

        cy.get('#selectPassage').select('P1 - C2')

        cy.wait('@getFloorsPhysics')
        cy.wait('@getFloorsChemistry')

        cy.get('#newBuildingCode1').should('have.value', 'P')
        cy.get('#newBuildingCode2').should('have.value', 'C')
        cy.get('#newFloorNumber1').select('1')

        cy.get('button[type="submit"]').should('be.disabled')

        cy.get('#newFloorNumber2').select('1')

        cy.intercept('PATCH', 'http://localhost:4000/api/passages', {
            statusCode: 201,
            body: { success: true },
        }).as('patchPassage')

        cy.get('button[type="submit"]').should('not.be.disabled').click()

        cy.wait('@patchPassage').its('response.statusCode').should('eq', 201)
    })
})
