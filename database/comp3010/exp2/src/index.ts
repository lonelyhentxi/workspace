import { isInteger, isNil } from 'lodash';
import mysql from 'mysql';
import { type } from "os";

const argv = require('yargs').argv;

const connection = mysql.createConnection({
    host: 'localhost',
    user: 'root',
    database: 'company',
    password: 'Zh123123en'
});
connection.connect((err) => {
    if (err) {
        console.log(`无法连接数据库: ${err}`);
    } else {
        console.log('连接到数据库');
    }
});


const option = {question: argv.q - 1, params: argv.q};
const questionId: number = option['question'];

if (!isInteger(questionId) || questionId < 0 || questionId > 3) {
    console.log(`你的问题序号是${questionId + 1}, 但是应在1-4之间.`);
}
if (isNil(option['params'])) {
    console.log(`请输入json格式的参数`);
}
try {
    option.params = JSON.parse(argv.p);
} catch {
    console.log(`请输入json格式的参数`);
}
const actions = [
    (queries: { pno: string }) => {
        if((typeof queries.pno) != 'string') {
            console.log(`请输入正确的pno`);
            connection.end(()=>{});
        }
        connection.query(
            'SELECT essn FROM works_on WHERE pno = ?',
            [queries.pno],
            function (err, results) {
                console.log('查询结果为：');
                for (const res of results) {
                    console.log(`${res.essn}工作于项目${queries.pno}。`);
                }
                connection.end(()=>{});
            }
        );
    },
    (queries: { pname: string }) => {
        if((typeof queries.pname) != 'string') {
            console.log(`请输入正确的pname`);
            connection.end(()=>{});
        }
        connection.query(
            `select distinct ename from EMPLOYEE where essn in (select distinct essn from works_on natural join project where pname = ?)`,
            [queries.pname],
            function (err, results) {
                if (err) {
                    console.log('查询失败');
                }
                console.log('查询结果为：');
                for (const res of results) {
                    console.log(`${res.ename}工作于项目${queries.pname}。`);
                }
                connection.end(()=>{});
            }
        );
    },
    (queries: { dname: string }) => {
        if((typeof queries.dname) != 'string') {
            console.log(`请输入正确的dname`);
            connection.end(()=>{});
        }
        connection.query(
            'SELECT DISTINCT ename, address FROM employee NATURAL JOIN department WHERE dname = ?',
            [queries.dname],
            function (err, results) {
                if (err) {
                    console.log('查询失败');
                }
                console.log('查询结果为：');
                for (const res of results) {
                    console.log(`名为${res.ename}，地址为${res.address}的员工，工作于${queries.dname}。`);
                }
                connection.end(()=>{});
            }
        );
    },
    (queries: { dname: string, salary: number }) => {
        if((typeof queries.dname) != 'string') {
            console.log(`请输入正确的dname`);
            connection.end(()=>{});
        }
        if(!isInteger(queries.salary)) {
            console.log(`请输入正确的salary`);
            connection.end(()=>{});
        }
        connection.query(
            'SELECT DISTINCT ename, address FROM employee NATURAL JOIN department WHERE dname = ? AND salary < ?',
            [queries.dname, queries.salary],
            function (err, results) {
                if (err) {
                    console.log('查询失败');
                }
                console.log('查询结果为：');
                for (const res of results) {
                    console.log(`名为${res.ename}，地址为${res.address}的员工，工作于${queries.dname},且薪水小于${queries.salary}`);
                }
                connection.end(()=>{});
            }
        );
    }
];
// @ts-ignore
actions[questionId](option.params);

