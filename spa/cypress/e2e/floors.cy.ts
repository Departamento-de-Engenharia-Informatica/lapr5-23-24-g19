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

describe('Floors e2e tests', () => {
    beforeEach(() => {
        cy.intercept('GET', `http://localhost:4000/api/buildings`, {
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

    })

    describe('List floors e2e tests', () => {
        beforeEach(() => {

            cy.visit('campus/floors/list')
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
            cy.title().should('equal', 'List Floors')
        })

        it('should have an empty selected building', () => {
            cy.get('#building').should('have.value', null)
        })

        it('should initially have an empty floor list', () => {
            cy.get('.floor-card').should('not.exist')
        })

        it('should select a building and display floors', () => {
            cy.wait('@getBuildings')
            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: 2,
                        description: 'Physics Labs',
                    },
                ],
            }).as('getFloorsPhysics')

            cy.get('#building').select('P')

            cy.wait('@getFloorsPhysics')

            cy.get('.floor-card').should('exist')
            cy.get('.floor-card').should('contain.text', 'Floor 2')
            cy.get('.floor-card').should('contain.text', 'Physics Labs')

            cy.intercept('GET', 'http://localhost:4000/api/buildings/C/floors', {
                body: [
                    {
                        buildingCode: 'C',
                        floorNumber: 1,
                        description: 'Chemistry Labs',
                    },
                    {
                        buildingCode: 'C',
                        floorNumber: 2,
                        description: 'Chemistry Research',
                    },
                ],
            }).as('getFloorsChemistry')

            cy.get('#building').select('C')
            cy.wait('@getFloorsChemistry')

            cy.get('.floor-card').should('exist')
            cy.get('.floor-card').should('contain.text', 'Floor 1')
            cy.get('.floor-card').should('contain.text', 'Chemistry Labs')
            cy.get('.floor-card').should('contain.text', 'Floor 2')
            cy.get('.floor-card').should('contain.text', 'Chemistry Research')
        })

        it('should handle floors with empty descriptions', () => {
            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: 2,
                        description: '',
                    },
                ],
            }).as('getFloorsEmptyDescription')

            cy.get('#building').select('P')

            cy.wait('@getFloorsEmptyDescription')

            cy.get('.floor-card').should('exist')
            cy.get('.floor-card').should('contain.text', 'No description')
        })
    })

    describe('Create Floors e2e tests', () => {
        beforeEach(() => {
            cy.visit('campus/floors/create')
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

        it('should have the correct title', () => {
            cy.title().should('equal', 'Create Floor')
        })

        it('should display the form elements', () => {
            cy.get('#selectedBuilding').should('exist')
            cy.get('#floorNumber').should('exist')
            cy.get('#description').should('exist')
            cy.get('button[type="submit"]').should('exist')
        })

        it('should display a list of buildings in the select dropdown', () => {
            cy.get('#selectedBuilding').select('P').should('have.value', 'P')
            cy.get('#selectedBuilding').select('C').should('have.value', 'C')
        })

        it('should display existing floors when a building is selected', () => {
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
                        description: 'Physics Research',
                    },
                ],
            }).as('getExistingFloors')

            cy.get('#selectedBuilding').select('P')
            cy.wait('@getExistingFloors')

            cy.get('.floor-card').should('have.length', 2)
            cy.get('.floor-card').should('contain.text', 'Floor 1')
            cy.get('.floor-card').should('contain.text', 'Physics Labs')
            cy.get('.floor-card').should('contain.text', 'Floor 2')
            cy.get('.floor-card').should('contain.text', 'Physics Research')
        })

        it('should be able to create a new floor', () => {
            const floorNumber = '3'
            const description = 'New Floor'

            cy.get('#selectedBuilding').select('P')
            cy.get('#floorNumber').type(floorNumber)
            cy.get('#description').type(description)

            cy.intercept('POST', 'http://localhost:4000/api/buildings/P/floors', {
                statusCode: 201,
                body: {
                    buildingCode: 'P',
                    floorNumber: floorNumber,
                    description: description,
                },
            }).as('createFloor')

            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: floorNumber,
                        description: description,
                    },
                ],
            }).as('getFloors')

            cy.get('button[type="submit"]').click()
            cy.wait('@createFloor')
            cy.wait('@getFloors')

            cy.get('.floor-card').should('exist')
            cy.get('.floor-card').should('contain.text', `Floor ${floorNumber}`)
            cy.get('.floor-card').should('contain.text', description)
        })

        it('should handle form validation', () => {
            const floorNumber = '3'
            const description = 'New Floor'

            cy.get('button[type="submit"]').should('be.disabled')

            cy.get('#selectedBuilding').should('have.class', 'ng-invalid')
            cy.get('#floorNumber').should('have.class', 'ng-invalid')

            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: floorNumber,
                        description: description,
                    },
                ],
            }).as('getFloors')

            cy.get('#selectedBuilding').select('P')
            cy.wait('@getFloors')
            cy.get('button[type="submit"]').should('be.disabled')

            cy.get('#description').type(
                'This is optional but the form should still be disabled without typing the floor number',
            )
            cy.get('button[type="submit"]').should('be.disabled')

            cy.get('#floorNumber').type('3')
            cy.get('button[type="submit"]').should('not.be.disabled')
        })

        it('should reset the form after floor creation', () => {
            const floorNumber = '3'
            const description = 'New Floor'

            cy.get('#selectedBuilding').select('P')
            cy.get('#floorNumber').type(floorNumber)
            cy.get('#description').type(description)

            cy.intercept('POST', 'http://localhost:4000/api/buildings/P/floors', {
                statusCode: 201,
                body: {
                    buildingCode: 'P',
                    floorNumber: floorNumber,
                    description: description,
                },
            }).as('createFloor')

            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: floorNumber,
                        description: description,
                    },
                ],
            }).as('getFloors')

            cy.get('button[type="submit"]').click()
            cy.wait('@createFloor')
            cy.wait('@getFloors')

            cy.get('#selectedBuilding').should('have.value', 'P')
            cy.get('#floorNumber').should('have.value', '')
            cy.get('#description').should('have.value', '')
        })
    })
    describe('Edit floors e2e tests', () => {
        beforeEach(() => {
            cy.visit('campus/floors/edit')
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

            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: 0,
                        description: 'Community floor',
                    },
                    {
                        buildingCode: 'P',
                        floorNumber: 1,
                        description: 'Classrooms',
                    },
                    {
                        buildingCode: 'P',
                        floorNumber: 2,
                        description: 'Physics Labs',
                    },
                ],
            }).as('getFloorsPhysics')

            cy.intercept('GET', 'http://localhost:4000/api/buildings/C/floors', {
                body: [
                    {
                        buildingCode: 'C',
                        floorNumber: 0,
                        description: 'Bar',
                    },
                    {
                        buildingCode: 'C',
                        floorNumber: 1,
                        description: 'Labs',
                    },
                    {
                        buildingCode: 'C',
                        floorNumber: 2,
                        description: 'Teachers room',
                    },
                ],
            }).as('getFloorsChemistry')
        })

        it('should have the correct title', () => {
            cy.title().should('equal', 'Edit Floor')
        })

        it('should display the form elements', () => {
            cy.get('#selectBuilding').should('exist')
            cy.get('#selectFloor').should('exist')
            cy.get('#newFloorNumber').should('exist')
            cy.get('#description').should('exist')
            cy.get('#overrideConfirmation').should('exist')
            cy.get('button[type="submit"]').should('exist')
        })

        it('should display existing floors when a building is selected', () => {
            cy.wait('@getBuildings')

            cy.get('#selectBuilding').select('P')
            cy.wait('@getFloorsPhysics')

            cy.get('.floor-card').should('have.length', 3)
            cy.get('.floor-card').should('contain.text', 'Community floor')
            cy.get('.floor-card').should('contain.text', 'Classrooms')
            cy.get('.floor-card').should('contain.text', 'Physics Labs')

            cy.get('#selectBuilding').select('C')
            cy.wait('@getFloorsChemistry')

            cy.get('.floor-card').should('have.length', 3)
            cy.get('.floor-card').should('contain.text', 'Bar')
            cy.get('.floor-card').should('contain.text', 'Labs')
            cy.get('.floor-card').should('contain.text', 'Teachers room')
        })

        it('should display placeholders when a floor is selected for editing', () => {
            cy.wait('@getBuildings')

            cy.get('#selectBuilding').select('C')
            cy.wait('@getFloorsChemistry')

            cy.get('#selectFloor').select('2')

            cy.get('#newFloorNumber').should('have.attr', 'placeholder', '2')
            cy.get('#description').should('have.attr', 'placeholder', 'Teachers room')
        })

        it('should update form placeholders when a different floor is selected', () => {
            cy.wait('@getBuildings')

            cy.get('#selectBuilding').select('C')
            cy.wait('@getFloorsChemistry')

            cy.get('#selectFloor').select('1')

            cy.get('#newFloorNumber').should('have.attr', 'placeholder', '1')
            cy.get('#description').should('have.attr', 'placeholder', 'Labs')

            cy.get('#selectFloor').select('2')

            cy.get('#newFloorNumber').should('have.attr', 'placeholder', '2')
            cy.get('#description').should('have.attr', 'placeholder', 'Teachers room')
        })

        it('should update form placeholders when floor is changed', () => {
            cy.wait('@getBuildings')

            cy.get('#selectBuilding').select('C')
            cy.wait('@getFloorsChemistry')

            cy.get('#selectFloor').select('2')

            cy.get('#newFloorNumber').should('have.attr', 'placeholder', '2')
            cy.get('#description').should('have.attr', 'placeholder', 'Teachers room')

            cy.get('#selectFloor').select('1')

            cy.get('#newFloorNumber').should('have.attr', 'placeholder', '1')
            cy.get('#description').should('have.attr', 'placeholder', 'Labs')
        })

        it('should display confirmation checkbox unchecked by default', () => {
            cy.get('#overrideConfirmation').should('not.be.checked')
        })

        it('should successfully update floor description without override', () => {
            cy.wait('@getBuildings')

            cy.get('#selectBuilding').select('P')
            cy.wait('@getFloorsPhysics')

            cy.get('#selectFloor').select('2')

            cy.get('#newFloorNumber').should('have.attr', 'placeholder', '2')
            cy.get('#description').should('have.attr', 'placeholder', 'Physics Labs')

            const updatedDescription = 'Updated Physics Labs'
            cy.get('#description').clear().type(updatedDescription)

            cy.intercept('PATCH', 'http://localhost:4000/api/buildings/P/floors/2', {
                statusCode: 200,
                body: {
                    buildingCode: 'P',
                    floorNumber: 2,
                    description: 'Updated Physics Labs',
                },
            }).as('updateFloor')

            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: 0,
                        description: 'Community floor',
                    },
                    {
                        buildingCode: 'P',
                        floorNumber: 1,
                        description: 'Classrooms',
                    },
                    {
                        buildingCode: 'P',
                        floorNumber: 2,
                        description: 'Updated Physics Labs',
                    },
                ],
            }).as('getFloorsPhysics')

            cy.get('button[type="submit"]').should('not.be.disabled').click()

            cy.wait('@updateFloor')
            cy.wait('@getFloorsPhysics')

            cy.get('.floor-card').should('contain.text', `Floor 2`)
            cy.get('.floor-card').should('contain.text', `Updated Physics Labs`)
        })

        it('should successfully update floor number without override', () => {
            cy.wait('@getBuildings')

            cy.get('#selectBuilding').select('P')
            cy.wait('@getFloorsPhysics')

            cy.get('#selectFloor').select('2')

            cy.get('#newFloorNumber').should('have.attr', 'placeholder', '2')
            cy.get('#description').should('have.attr', 'placeholder', 'Physics Labs')

            const updatedFloorNumber = '3'
            cy.get('#newFloorNumber').clear().type(updatedFloorNumber)

            cy.intercept('PATCH', 'http://localhost:4000/api/buildings/P/floors/2', {
                statusCode: 200,
                body: {
                    buildingCode: 'P',
                    floorNumber: 3,
                    description: 'Physics Labs',
                },
            }).as('updateFloorNumber')

            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: 0,
                        description: 'Community floor',
                    },
                    {
                        buildingCode: 'P',
                        floorNumber: 1,
                        description: 'Classrooms',
                    },
                    {
                        buildingCode: 'P',
                        floorNumber: 3,
                        description: 'Physics Labs',
                    },
                ],
            }).as('getFloorsPhysics')

            cy.get('button[type="submit"]').should('not.be.disabled').click()

            cy.wait('@updateFloorNumber')
            cy.wait('@getFloorsPhysics')

            cy.get('.floor-card').should('contain.text', `Floor 3`)
            cy.get('.floor-card').should('contain.text', `Physics Labs`)
        })

        it('should successfully update floor number and description without override', () => {
            cy.wait('@getBuildings')

            cy.get('#selectBuilding').select('P')
            cy.wait('@getFloorsPhysics')

            cy.get('#selectFloor').select('2')

            cy.get('#newFloorNumber').should('have.attr', 'placeholder', '2')
            cy.get('#description').should('have.attr', 'placeholder', 'Physics Labs')

            const updatedFloorNumber = '3'
            const updatedDescription = 'Updated Physics Labs'

            cy.get('#newFloorNumber').clear().type(updatedFloorNumber)
            cy.get('#description').clear().type(updatedDescription)

            cy.intercept('PATCH', 'http://localhost:4000/api/buildings/P/floors/2', {
                statusCode: 200,
                body: {
                    buildingCode: 'P',
                    floorNumber: 3,
                    description: 'Updated Physics Labs',
                },
            }).as('updateFloor')

            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: 0,
                        description: 'Community floor',
                    },
                    {
                        buildingCode: 'P',
                        floorNumber: 1,
                        description: 'Classrooms',
                    },
                    {
                        buildingCode: 'P',
                        floorNumber: 3,
                        description: 'Updated Physics Labs',
                    },
                ],
            }).as('getFloorsPhysics')

            cy.get('button[type="submit"]').should('not.be.disabled').click()

            cy.wait('@updateFloor')
            cy.wait('@getFloorsPhysics')

            cy.get('.floor-card').should('contain.text', `Floor 3`)
            cy.get('.floor-card').should('contain.text', `Updated Physics Labs`)
        })

        it('should not allow updating only description with override', () => {
            cy.wait('@getBuildings')

            cy.get('#selectBuilding').select('P')
            cy.wait('@getFloorsPhysics')

            cy.get('#selectFloor').select('2')

            cy.get('#newFloorNumber').should('have.attr', 'placeholder', '2')
            cy.get('#description').should('have.attr', 'placeholder', 'Physics Labs')

            const updatedDescription = 'Updated Physics Labs'

            cy.get('#description').clear().type(updatedDescription)
            cy.get('#overrideConfirmation').check()

            cy.get('button[type="submit"]').should('be.disabled')
        })

        it('should successfully update floor number with override', () => {
            cy.wait('@getBuildings')

            cy.get('#selectBuilding').select('P')
            cy.wait('@getFloorsPhysics')

            cy.get('#selectFloor').select('2')

            cy.get('#newFloorNumber').should('have.attr', 'placeholder', '2')
            cy.get('#description').should('have.attr', 'placeholder', 'Physics Labs')

            const updatedFloorNumber = '3'

            cy.get('#newFloorNumber').clear().type(updatedFloorNumber)
            cy.get('#overrideConfirmation').check()

            cy.intercept('PUT', 'http://localhost:4000/api/buildings/P/floors/2', {
                statusCode: 200,
                body: {
                    buildingCode: 'P',
                    floorNumber: 3,
                },
            }).as('updateFloorNumber')

            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: 0,
                        description: 'Community floor',
                    },
                    {
                        buildingCode: 'P',
                        floorNumber: 1,
                        description: 'Classrooms',
                    },
                    {
                        buildingCode: 'P',
                        floorNumber: 3,
                    },
                ],
            }).as('getFloorsPhysics')

            cy.get('button[type="submit"]').should('not.be.disabled').click()

            cy.wait('@updateFloorNumber')
            cy.wait('@getFloorsPhysics')

            cy.get('.floor-card').should('contain.text', `Floor 3`)
            cy.get('.floor-card').should('contain.text', `No description`)
        })

        it('should successfully update floor number and description with override', () => {
            cy.wait('@getBuildings')

            cy.get('#selectBuilding').select('P')
            cy.wait('@getFloorsPhysics')

            cy.get('#selectFloor').select('2')

            cy.get('#newFloorNumber').should('have.attr', 'placeholder', '2')
            cy.get('#description').should('have.attr', 'placeholder', 'Physics Labs')

            const updatedFloorNumber = '3'
            const updatedDescription = 'Updated Physics Labs'

            cy.get('#newFloorNumber').clear().type(updatedFloorNumber)
            cy.get('#description').clear().type(updatedDescription)
            cy.get('#overrideConfirmation').check()

            cy.intercept('PUT', 'http://localhost:4000/api/buildings/P/floors/2', {
                statusCode: 200,
                body: {
                    buildingCode: 'P',
                    floorNumber: 3,
                    description: 'Updated Physics Labs',
                },
            }).as('updateFloorNumberAndDescription')

            cy.intercept('GET', 'http://localhost:4000/api/buildings/P/floors', {
                body: [
                    {
                        buildingCode: 'P',
                        floorNumber: 0,
                        description: 'Community floor',
                    },
                    {
                        buildingCode: 'P',
                        floorNumber: 1,
                        description: 'Classrooms',
                    },
                    {
                        buildingCode: 'P',
                        floorNumber: 3,
                        description: 'Updated Physics Labs',
                    },
                ],
            }).as('getFloorsPhysics')

            cy.get('button[type="submit"]').should('not.be.disabled').click()

            cy.wait('@updateFloorNumberAndDescription')
            cy.wait('@getFloorsPhysics')

            cy.get('.floor-card').should('contain.text', `Floor 3`)
            cy.get('.floor-card').should('contain.text', `Updated Physics Labs`)
        })
    })
})
