// function loginViaAuth0Ui(username: string, password: string) {
//     cy.origin(
//         Cypress.env('auth_domain'),
//         { args: { username, password } },
//         ({ username, password }) => {
//             cy.get('input#1-email').type(username)
//             cy.get('input#1-password').type(password, { log: false })
//             cy.get('button[type="submit"]')
//                 .should('be.visible')
//                 .should('contain.text', 'Log In')
//                 .should('not.be.disabled')
//                 .should('not.be.hidden')
//                 .click()
//         },
//     )
// }
//
// describe('List Passages e2e tests', () => {
//     beforeEach(() => {
//         cy.intercept('GET', 'http://localhost:4000/api/buildings', {
//             body: [
//                 {
//                     code: 'P',
//                     name: 'Civil2',
//                     description: 'Departamento de Civil2',
//                     maxFloorDimensions: {
//                         length: 300,
//                         width: 250,
//                     },
//                 },
//                 {
//                     code: 'O',
//                     name: 'Informatic',
//                     description: 'Informatic Department',
//                     maxFloorDimensions: {
//                         length: 20,
//                         width: 30,
//                     },
//                 },
//             ],
//         }).as('getBuildings')
//
//         cy.visit('/campus/passages/list')
//         const log = Cypress.log({
//             displayName: 'AUTH0 LOGIN',
//             message: [`🔐 Authenticating | ${Cypress.env('auth_username')}`],
//             // @ts-ignore
//             autoEnd: false,
//         })
//         log.snapshot('before')
//
//         loginViaAuth0Ui(Cypress.env('auth_username'), Cypress.env('auth_password'))
//
//         log.snapshot('after')
//         log.end()
//     })
//
//     it('has the correct title', () => {
//         cy.title().should('equal', 'List passages between buildings')
//     })
//
//     it('should have an empty selected buildings', () => {
//         cy.get('#building').should('have.value', null)
//         cy.get('#building1').should('have.value', null)
//     })
//
//     it('should initially have an empty passages list', () => {
//         cy.get('.list-passages-between-buildings-card').should('not.exist')
//     })
//
//     it('should select buildings and display passages', () => {
//         cy.wait('@getBuildings')
//         cy.intercept(
//             'GET',
//             'http://localhost:4000/api/passages/?building1=P&building2=O',
//             {
//                 body: [
//                     {
//                         floor1: {
//                             buildingCode: 'O',
//                             floorNumber: 2,
//                         },
//                         floor2: {
//                             buildingCode: 'P',
//                             floorNumber: 3,
//                         },
//                     },
//                 ],
//             },
//         ).as('getPassagesPO')
//
//         cy.get('#building').select('P')
//
//         cy.get('#building1').select('O')
//         cy.wait('@getPassagesPO')
//
//         cy.get('.list-passages-between-buildings-card').should('exist')
//         cy.get('h2').should('contain.text', 'Passage')
//         cy.get('p').should('contain.text', 'buildingCode: O')
//         cy.get('p').should('contain.text', 'floorNumber: 2')
//         cy.get('p').should('contain.text', 'buildingCode: P')
//         cy.get('p').should('contain.text', 'floorNumber: 3')
//     })
//
//     it('should handle multiple passages', () => {
//         cy.intercept(
//             'GET',
//             'http://localhost:4000/api/passages/?building1=P&building2=O',
//             {
//                 body: [
//                     {
//                         floor1: {
//                             buildingCode: 'O',
//                             floorNumber: 2,
//                         },
//                         floor2: {
//                             buildingCode: 'P',
//                             floorNumber: 3,
//                         },
//                     },
//                     {
//                         floor1: {
//                             buildingCode: 'O',
//                             floorNumber: 2,
//                         },
//                         floor2: {
//                             buildingCode: 'P',
//                             floorNumber: 1,
//                         },
//                     },
//                     {
//                         floor1: {
//                             buildingCode: 'O',
//                             floorNumber: 1,
//                         },
//                         floor2: {
//                             buildingCode: 'P',
//                             floorNumber: 2,
//                         },
//                     },
//                     {
//                         floor1: {
//                             buildingCode: 'O',
//                             floorNumber: 4,
//                         },
//                         floor2: {
//                             buildingCode: 'P',
//                             floorNumber: 5,
//                         },
//                     },
//                 ],
//             },
//         ).as('getMultiplePassages')
//
//         cy.get('#building').select('P')
//         cy.get('#building1').select('O')
//         cy.wait('@getMultiplePassages')
//
//         cy.get('.list-passages-between-buildings-card').should('exist')
//
//         cy.get('h2').should('contain.text', 'Passage')
//
//         cy.get('p').should('contain.text', 'buildingCode: O')
//         cy.get('p').should('contain.text', 'floorNumber: 2')
//         cy.get('p').should('contain.text', 'buildingCode: P')
//         cy.get('p').should('contain.text', 'floorNumber: 3')
//
//         cy.get('p').should('contain.text', 'buildingCode: O')
//         cy.get('p').should('contain.text', 'floorNumber: 2')
//         cy.get('p').should('contain.text', 'buildingCode: P')
//         cy.get('p').should('contain.text', 'floorNumber: 1')
//
//         cy.get('p').should('contain.text', 'buildingCode: O')
//         cy.get('p').should('contain.text', 'floorNumber: 1')
//         cy.get('p').should('contain.text', 'buildingCode: P')
//         cy.get('p').should('contain.text', 'floorNumber: 2')
//
//         cy.get('p').should('contain.text', 'buildingCode: O')
//         cy.get('p').should('contain.text', 'floorNumber: 4')
//         cy.get('p').should('contain.text', 'buildingCode: P')
//         cy.get('p').should('contain.text', 'floorNumber: 5')
//     })
// })
