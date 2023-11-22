
describe('Floors e2e tests', () => {
    beforeEach(() => {
        cy.intercept('GET', `http://localhost:4000/status`, {
            statusCode:200
        }).as('getBuildings')

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

        cy.visit('/floors/list')
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