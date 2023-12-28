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

describe('Elevator Form e2e tests', () => {
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
        cy.visit('/campus/elevators/create')

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
        cy.title().should('equal', 'Create Elevator')
    })

    it('should have empty initial values', () => {
        cy.get('#selectedBuilding').should('have.value', null)
        cy.get('#selectedFloors').invoke('val').should('deep.equal', [])
        cy.get('#brand').should('have.value', '')
        cy.get('#model').should('have.value', '')
        cy.get('#serialNumber').should('have.value', '')
        cy.get('#description').should('have.value', '')
    })

    /*  it('should display error and disable submit button for invalid form', () => {
        // Submit the form without filling in any fields
        cy.get('button[type="submit"]').click({ force: true });

        // Wait for the error message to appear
        cy.get('.error-message', { timeout: 5000 }).should('exist'); // Increase timeout as needed

        // Assert disabled submit button
        cy.get('button[type="submit"]').should('be.disabled');
    });
*/

    it('should submit the form successfully and display created elevator', () => {
        cy.wait('@getBuildings')

        cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
            body: [
                {
                    buildingCode: 'P',
                    floorNumber: 1,
                    description: 'Physics Labs',
                },
                {
                    buildingCode: 'P',
                    floorNumber: 2,
                    description: 'Physics Labs',
                },
                {
                    buildingCode: 'P',
                    floorNumber: 3,
                    description: 'Physics Labs',
                },
            ],
        }).as('getFloors')

        cy.get('#selectedBuilding').select('P')
        cy.wait('@getFloors')

        const buildingId = 'P'
        const identifier = 1
        const floorsNumber = [1, 2]
        const brand = 'BrandName'
        const model = 'ModelName'
        const serialNumber = 'Serial123'
        const description = 'This is a test elevator.'

        cy.get('#selectedFloors').select(floorsNumber)
        cy.get('#brand').type(brand)
        cy.get('#model').type(model)
        cy.get('#serialNumber').type(serialNumber)
        cy.get('#description').type(description)

        cy.intercept('POST', 'http://localhost:4000/api/buildings/P/elevators', {
            statusCode: 201,
            body: {
                buildingId: buildingId,
                identifier: 1,
                floors: floorsNumber,
                brand: brand,
                model: model,
                serialNumber: serialNumber,
                description: description,
            },
        }).as('createElevator')

        cy.get('button[type="submit"]').click()
        cy.wait('@createElevator')

        cy.get('.elevator-list').should('exist')
        cy.get('.elevator-card p').should('have.length', 7)

        cy.get('.elevator-card p').contains('Building ID: P')
        cy.get('.elevator-card p').contains('Identifier: 1')
        cy.get('.elevator-card p').contains('Floors: 1, 2')
        cy.get('.elevator-card p').contains('Brand: BrandName')
        cy.get('.elevator-card p').contains('Model: ModelName')
        cy.get('.elevator-card p').contains('Serial Number: Serial123')
        cy.get('.elevator-card p').contains('Description: This is a test elevator.')
    })
})
